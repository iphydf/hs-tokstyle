{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.C.Linter.CallbackDiscipline (descr) where

import           Control.Monad                   (unless, when)
import           Data.Functor.Identity           (Identity)
import           Data.List                       (intercalate, isInfixOf,
                                                  isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (mapMaybe)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemRep      (CompTypeRef (..), FunDef (..),
                                                  FunType (..),
                                                  GlobalDecls (..),
                                                  IdentDecl (..),
                                                  ParamDecl (..), Type (..),
                                                  TypeName (..), VarDecl (..),
                                                  VarName (..))
import           Language.C.Analysis.TravMonad   (Trav, TravT)
import           Language.C.Analysis.TypeUtils   (canonicalType)
import           Language.C.Data.Ident           (Ident (..), SUERef (..))
import           Language.C.Data.Node            (NodeInfo)
import qualified Language.C.Pretty               as C
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants     (CInteger (..))
import           Prettyprinter                   (pretty, (<+>))
import qualified Tokstyle.C.Env                  as Env
import           Tokstyle.C.Env                  (DiagnosticLevel (..), Env,
                                                  recordLinterError,
                                                  recordRichError)
import           Tokstyle.C.ObjectSystem         (CallbackSlot (..),
                                                  ObjectInfo (..),
                                                  discoverObjectTypes,
                                                  isCallbackMember, isFuncPtr,
                                                  isUserdataMember)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)
import           Tokstyle.C.TravUtils            (backticks)


-- | Analyze a function body to see if it behaves like a registry.
isRegistryBehavior :: Map.Map String ObjectInfo -> FunDef -> Set CallbackSlot
isRegistryBehavior objs (FunDef (VarDecl _ _ (FunctionType (FunType _ params _) _)) body _) =
    let
        paramInfos = mapMaybe getParamInfo params
        getParamInfo (ParamDecl (VarDecl (VarName (Ident n _ _) _) _ ty) _) = Just (n, ty)
        getParamInfo _ = Nothing

        paramNames = map fst paramInfos

        -- Check if an expression is a parameter.
        isParam (CVar (Ident n _ _) _) = n `elem` paramNames
        isParam _                      = False

        -- Resolve the struct type of a variable if it's a parameter.
        getStructType (CVar (Ident n _ _) _) =
            case lookup n paramInfos of
                Just (canonicalType -> PtrType (DirectType (TyComp (CompTypeRef (NamedRef (Ident s _ _)) _ _)) _ _) _ _) -> Just s
                _ -> Nothing
        getStructType _ = Nothing

        -- Check if a member access is to a callback field of a known object type.
        getCallbackSlot (CMember structExpr (Ident fieldName _ _) _ _) =
            case getStructType structExpr of
                Just s  -> if isCallbackMember objs s fieldName || isUserdataMember objs s fieldName
                           then Just (CallbackSlot s fieldName)
                           else Nothing
                Nothing -> Nothing
        getCallbackSlot (CIndex l _ _) = getCallbackSlot l
        getCallbackSlot _ = Nothing

    in isRegistryStat isParam getCallbackSlot body
isRegistryBehavior _ _ = Set.empty


isRegistryStat :: (CExpr -> Bool) -> (CExpr -> Maybe CallbackSlot) -> CStat -> Set CallbackSlot
isRegistryStat isParam getSlot stat = case stat of
    CExpr (Just (CAssign _ l r _)) _ ->
        if (isParam r || isNull r)
        then case getSlot l of
            Just s  -> Set.singleton s
            Nothing -> Set.empty
        else Set.empty
    CIf _ t e _ -> isRegistryStat isParam getSlot t `Set.union` maybe Set.empty (isRegistryStat isParam getSlot) e
    CFor _ _ _ s _ -> isRegistryStat isParam getSlot s
    CWhile _ s _ _ -> isRegistryStat isParam getSlot s
    CCompound _ items _ -> Set.unions $ map (isRegistryItem isParam getSlot) items
    _ -> Set.empty
  where
    isRegistryItem p c (CBlockStmt s) = isRegistryStat p c s
    isRegistryItem _ _ _              = Set.empty



isNull :: CExpr -> Bool
isNull (CVar (Ident n _ _) _)                  = n == "NULL" || n == "nullptr"
isNull (CConst (CIntConst (CInteger 0 _ _) _)) = True
isNull _                                       = False


countComplexity :: CStat -> Int
countComplexity (CCompound _ items _) = sum $ map countItem items
  where
    countItem (CBlockStmt s) = countStat s
    countItem (CBlockDecl _) = 1
    countItem _              = 0

    countStat (CCompound _ items' _) = sum $ map countItem items'
    countStat (CIf _ t e _)          = 1 + countStat t + maybe 0 countStat e
    countStat (CFor _ _ _ s _)       = 1 + countStat s
    countStat (CWhile _ s _ _)       = 1 + countStat s
    countStat (CExpr _ _)            = 1
    countStat (CReturn _ _)          = 1
    countStat _                      = 1
countComplexity _ = 1


isAllowedCall :: String -> Bool
isAllowedCall name = name `elem` ["assert", "__assert_fail", "memcpy", "memset", "tox_lock", "tox_unlock", "pthread_mutex_lock", "pthread_mutex_unlock", "random_u64", "UINT32_C", "UINT16_C", "UINT8_C"]


checkCalls :: NodeInfo -> String -> CStat -> TravT Env Identity ()
checkCalls info funcName stat = checkStat stat
  where
    checkStat (CCompound _ items _) = mapM_ checkItem items
    checkStat (CIf _ t e _)          = checkStat t >> maybe (return ()) checkStat e
    checkStat (CFor _ _ _ s _)       = checkStat s
    checkStat (CWhile cond s isDo a) = do
        unless (isAssertLoop cond isDo) $
            recordLinterError a ("registry function" <+> backticks (pretty funcName) <+> "contains a loop")
        checkStat s
    checkStat (CExpr (Just e) _)     = checkExpr e
    checkStat _                      = return ()

    isAssertLoop cond True = isConstantZero cond
    isAssertLoop _ _       = False

    isConstantZero (CConst (CIntConst (CInteger 0 _ _) _)) = True
    isConstantZero _                                       = False

    checkItem (CBlockStmt s) = checkStat s
    checkItem _              = return ()

    checkExpr (CCall (CVar (Ident name _ _) _) _ _) =
        unless (isAllowedCall name) $
            recordRichError info ErrorLevel
                ("registry function" <+> backticks (pretty funcName) <+> "calls non-trivial function" <+> backticks (pretty name))
                []
                [(NoteLevel, "registry functions should be side-effect free and remain simple")]
    checkExpr (CCond c t e _) = checkExpr c >> maybe (return ()) checkExpr t >> checkExpr e
    checkExpr (CBinary _ l r _) = checkExpr l >> checkExpr r
    checkExpr (CUnary _ e _) = checkExpr e
    checkExpr (CAssign _ l r _) = checkExpr l >> checkExpr r
    checkExpr _ = return ()


checkRegistrySideEffects :: Map.Map String ObjectInfo -> FunDef -> TravT Env Identity ()
checkRegistrySideEffects objs (FunDef (VarDecl (VarName (Ident name _ _) _) _ _) body _) = do
    let
        -- Find all assignments.
        checkItem (CBlockStmt s) = checkStat s
        checkItem _              = return ()

        checkStat (CCompound _ items _) = mapM_ checkItem items
        checkStat (CIf _ t e _)          = checkStat t >> maybe (return ()) checkStat e
        checkStat (CFor _ _ _ s _)       = checkStat s
        checkStat (CWhile _ s _ _)       = checkStat s
        checkStat (CExpr (Just (CAssign _ l _ _)) _) = checkLHS l
        checkStat _                      = return ()

        checkLHS expr = case expr of
            CMember structExpr (Ident fieldName _ _) _ _ -> do
                ty <- tExpr [] RValue structExpr
                case canonicalType ty of
                    PtrType (DirectType (TyComp (CompTypeRef (NamedRef (Ident s _ _)) _ _)) _ _) _ _ ->
                        unless (isCallbackMember objs s fieldName || isUserdataMember objs s fieldName) $
                            recordError expr
                    DirectType (TyComp (CompTypeRef (NamedRef (Ident s _ _)) _ _)) _ _ ->
                        unless (isCallbackMember objs s fieldName || isUserdataMember objs s fieldName) $
                            recordError expr
                    _ -> return ()
            CIndex l _ _ -> checkLHS l
            _ -> return ()

        recordError l =
            recordRichError (annotation l) ErrorLevel
                ("registry function" <+> backticks (pretty name) <+> "mutates non-callback field" <+> backticks (pretty (show (C.pretty l))))
                []
                [(NoteLevel, "registry functions should only set callbacks and their associated userdata")]

    checkStat body
checkRegistrySideEffects _ _ = return ()


checkCallbackMutation :: Map.Map String ObjectInfo -> Map.Map CallbackSlot (Set String) -> CExpr -> TravT Env Identity ()
checkCallbackMutation objs registryMap expr = do
    mSlot <- getCallbackSlot expr
    case mSlot of
        Nothing -> return ()
        Just slot -> do
            ctx <- Env.getCtx
            let currentFunc = case [ f | f <- ctx, "func:" `isPrefixOf` f ] of
                    (f:_) -> drop 5 f
                    []    -> ""

            let allowed = Map.findWithDefault Set.empty slot registryMap

            unless (currentFunc `Set.member` allowed || isConstructor currentFunc || isDestructor currentFunc) $ do
                let msg = "mutation of callback/userdata field" <+> backticks (pretty (show (C.pretty expr))) <+> "outside of a registry function"
                let footers = if Set.null allowed
                              then []
                              else [(HelpLevel, "use one of these registry functions instead:" <+> pretty (intercalate ", " (Set.toList allowed)))]
                recordRichError (annotation expr) ErrorLevel msg [] footers
  where
    isConstructor f = "_new" `isInfixOf` f || "new_" `isInfixOf` f
    isDestructor f  = "_kill" `isInfixOf` f || "kill_" `isInfixOf` f

    getCallbackSlot (CMember structExpr (Ident fieldName _ _) _ _) = do
        ty' <- tExpr [] RValue structExpr
        case canonicalType ty' of
            PtrType (DirectType (TyComp (CompTypeRef (NamedRef (Ident s _ _)) _ _)) _ _) _ _ ->
                return $ if isCallbackMember objs s fieldName || isUserdataMember objs s fieldName
                         then Just (CallbackSlot s fieldName)
                         else Nothing
            DirectType (TyComp (CompTypeRef (NamedRef (Ident s _ _)) _ _)) _ _ ->
                return $ if isCallbackMember objs s fieldName || isUserdataMember objs s fieldName
                         then Just (CallbackSlot s fieldName)
                         else Nothing
            _ -> return Nothing
    getCallbackSlot (CIndex l _ _) = getCallbackSlot l
    getCallbackSlot _ = return Nothing


linter :: Map.Map String ObjectInfo -> Map.Map CallbackSlot (Set String) -> Set String -> AstActions (TravT Env Identity)
linter objs registryMap registries = astActions
    { doIdentDecl = \node act -> case node of
        FunctionDef f@(FunDef (VarDecl (VarName (Ident name _ _) _) _ _) body _) -> do
            let isRegistry = name `Set.member` registries
            when isRegistry $ do
                let complexity = countComplexity body
                when (complexity > 20) $
                    recordLinterError (annotation body) $
                        "registry function" <+> backticks (pretty name) <+> "is too complex (" <> pretty complexity <+> "statements)"

                checkCalls (annotation body) name body
                checkRegistrySideEffects objs f

            Env.pushCtx $ "func:" <> name
            act
            Env.popCtx
        _ -> act

    , doExpr = \node act -> case node of
        CAssign _ l _ _ -> do
            checkCallbackMutation objs registryMap l
            act
        _ -> act
    }


collectRegistries :: Map.Map String ObjectInfo -> GlobalDecls -> Map.Map CallbackSlot (Set String)
collectRegistries objs (GlobalDecls objs' _ _) =
    Map.unionsWith Set.union . mapMaybe isReg . Map.elems $ objs'
  where
    isReg (FunctionDef f@(FunDef (VarDecl (VarName (Ident name _ _) _) _ _) _ _)) =
        let slots = isRegistryBehavior objs f
        in if Set.null slots
           then Nothing
           else Just $ Map.fromList [ (s, Set.singleton name) | s <- Set.toList slots ]
    isReg _ = Nothing


analyse :: GlobalDecls -> Trav Env ()
analyse decls = do
    let objs = discoverObjectTypes decls
    let registryMap = collectRegistries objs decls
    let registries = Set.unions $ Map.elems registryMap
    traverseAst (linter objs registryMap registries) decls


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("callback-discipline", Text.unlines
    [ "Ensures callback discipline is followed."
    , ""
    , "Callback and userdata fields should only be mutated within registry"
    , "functions (functions that simply assign a parameter to a callback field)."
    , "Registry functions themselves must remain simple and side-effect free."
    ]))
