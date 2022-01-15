{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.VarUnusedInScope where

import           Control.Monad               (foldM)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFixM)
import           Data.List                   (delete, find)
import           Data.Text                   (Text)
import           Language.Cimple             (AssignOp (..), IdentityActions,
                                              Lexeme (..), Node, NodeF (..),
                                              UnaryOp (..), defaultActions,
                                              doNode, lexemeText, traverseAst)
import           Language.Cimple.Diagnostics (Diagnostics, warn)


data ReadKind
    = Bare
      -- Variable reference that may be a write if it's the lhs of an
      -- assignment.
    | Pure
      -- Pure read, i.e. if it's on the lhs of an assignment, the variable
      -- itself is not assigned, but e.g. is dereferenced and the pointee
      -- is assigned a new value.
    deriving (Show, Eq)

data Scope
    = Sibling
      -- The action is performed in the same scope as the current action.
    | Nested
      -- The action is performed in a nested scope.
    deriving (Show, Eq)

data Action
    = Declare
    | Write
    | Read ReadKind
    | WriteThenRead
    deriving (Show, Eq)

data Var = Var Action (Lexeme Text) Scope
  deriving (Show, Eq)

lookupVar :: Var -> [Var] -> Maybe Var
lookupVar = find . sameName
  where
    sameName (Var _ (L _ _ n1) _) (Var _ (L _ _ n2) _) = n1 == n2

combine :: [Var] -> [Var] -> [Var]
combine = foldr $ \var2@(Var act2 _ _) ls ->
    case lookupVar var2 ls of
        Nothing                            -> var2 : ls
        Just (Var act1 _ _) | act1 == act2 -> ls
        Just var1                          -> var2 : delete var1 ls

combineBranches :: [Var] -> [Var] -> [Var]
combineBranches ls1 ls2 = foldr join [] (ls1 ++ ls2)
  where
    join :: Var -> [Var] -> [Var]
    join var1 ls =
        case lookupVar var1 ls of
            Nothing   -> var1 : ls
            Just var2 ->
                let joined = joinVar var1 var2 in
                joined ++ delete var2 ls

    joinVar var1@(Var act1 _ _) (Var act2 _ _) | act1 == act2 = [var1]
    joinVar var1@(Var Read{} _ _) (Var Read{} _ _) = [var1]

    joinVar var1@(Var Read{} _ _) (Var Write _ _) = [var1]
    joinVar (Var Write _ _) var2@(Var Read{} _ _) = [var2]

    joinVar var1 var2 = error ("combineBranches" <> show (var1, var2))

combineStatements :: FilePath -> [Var] -> [Var] -> Diagnostics [Var]
combineStatements file ls1 ls2 = foldM join [] (ls1 ++ ls2)
  where
    join :: [Var] -> Var -> Diagnostics [Var]
    join ls var1 =
        case lookupVar var1 ls of
            Nothing   -> return $ var1 : ls
            Just var2 -> do
                joined <- joinVar var1 var2
                return $ joined ++ delete var2 ls

    joinVar var1@(Var Write l1 Sibling) (Var WriteThenRead _ Sibling) = do
        warn file l1 $ "value assigned to `" <> lexemeText l1 <> "' is never read"
        return [var1]

    joinVar var1@(Var Write l1 Sibling) (Var Write _ Sibling) = do
        warn file l1 $ "value assigned to `" <> lexemeText l1 <> "' is never read"
        return [var1]

    joinVar var1@(Var Read{} _ _) (Var Read{} _ _)        = return [var1]
    joinVar var1@(Var Read{} _ _) (Var Write _ _)         = return [var1]
    joinVar var1@(Var Read{} _ _) (Var WriteThenRead _ _) = return [var1]
    joinVar var1@(Var Write _ _)  (Var Write _ _)         = return [var1]
    joinVar (Var Write l1 s) (Var Read{} _ _)             = return [Var WriteThenRead l1 s]
    joinVar (Var Write l1 s) (Var WriteThenRead _ _)      = return [Var WriteThenRead l1 s]
    joinVar var1@(Var WriteThenRead _ _) _                = return [var1]

    joinVar (Var Declare l1 _) (Var Write l2 _) = do
        warn file l1 $ "variable `" <> lexemeText l1 <> "' can be reduced in scope"
        warn file l2 "  possibly to here"
        return []

    joinVar (Var Declare _ _) _ = return []
    joinVar var1@(Var _ l1 _) (Var Declare l2 _) = do
        warn file l1 $ "variable `" <> lexemeText l1 <> "' used before its declaration"
        warn file l2 $ "  `" <> lexemeText l2 <> "' was declared here"
        return [var1]


checkScopes :: FilePath -> NodeF (Lexeme Text) [Var] -> Diagnostics [Var]
checkScopes file = \case
    CompoundStmt ls             -> checkCompoundStmt ls
    ForStmt decl cont incr body -> return $ foldr combine [] [body, incr, cont, decl]
    IfStmt c t Nothing          -> return $ t ++ c
    IfStmt c t (Just e)         -> return $ combineBranches t e ++ c

    VarDecl t var a             -> return $ Var Declare var Sibling : foldr combine t a

    VarExpr var                 -> return [Var (Read Bare) var Sibling]
    AssignExpr lhs AopEq []     -> return $ map readToWrite lhs
    BinaryExpr lhs _ rhs        -> return $ combine lhs rhs
    UnaryExpr UopIncr expr      -> return $ map writeToRead expr
    UnaryExpr UopDecr expr      -> return $ map writeToRead expr
    FunctionCall f args         -> return $ foldr combine [] (f : args)

    UnaryExpr UopDeref e        -> return $ map readToPure e
    MemberAccess e _            -> return $ map readToPure e
    PointerAccess e _           -> return $ map readToPure e
    ArrayAccess e i             -> return $ map readToPure (e ++ i)

    PreprocIf _ t e             -> checkCompoundStmt $ e : t
    PreprocIfdef _ t e          -> checkCompoundStmt $ e : t
    PreprocIfndef _ t e         -> checkCompoundStmt $ e : t
    PreprocElse e               -> checkCompoundStmt e

    n                           -> return $ foldr combine [] n

  where
    checkCompoundStmt =
        fmap (map $ toNested . wtrToWrite) . foldM (combineStatements file) [] . reverse

    toNested (Var mode var _) = Var mode var Nested

    wtrToWrite (Var WriteThenRead var scope) = Var Write var scope
    wtrToWrite var                           = var

    readToPure (Var (Read Bare) var scope) = Var (Read Pure) var scope
    readToPure var                         = var

    readToWrite (Var (Read Bare) var scope) = Var Write var scope
    readToWrite var                         = var

    writeToRead (Var Write var scope)         = Var (Read Bare) var scope
    writeToRead (Var WriteThenRead var scope) = Var (Read Bare) var scope
    writeToRead var                           = var


linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDefn{} -> do
                _ <- foldFixM (checkScopes file) node
                return node

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
