{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.BufferSize (descr, looksAt) where

import           Control.Monad                 (forM_)
import           Data.List                     (sortOn)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (isJust, mapMaybe)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.C.Analysis.SemRep    (ArraySize (..), FunDef (..),
                                                GlobalDecls, IdentDecl (..),
                                                ObjDef (..), Type (..),
                                                VarDecl (..), VarName (..))
import           Language.C.Analysis.TravMonad (Trav, getUserState)
import           Language.C.Analysis.TypeUtils (canonicalType)
import           Language.C.Data.Ident         (identToString)
import           Language.C.Data.Node          (nameOfNode, posOfNode)
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants   (getCInteger)
import           Prettyprinter                 (Doc, pretty, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Tokstyle.C.Analysis.Interval  (Facts, IntTy (..), Range (..),
                                                Term (..), TermKind (..),
                                                boundedBy, eval, intType,
                                                subTerms, wrapSite)
import           Tokstyle.C.Analysis.Syntax    (exprChildren, localDecls,
                                                statExprs)
import           Tokstyle.C.Env                (Env (..))
import           Tokstyle.C.IntervalLinter     (analysedFunctions, exprDoc,
                                                recordWrap, valuesDoc)
import           Tokstyle.C.TravUtils          (backticks)


-- | The size in bytes of the memory an allocation returns.
allocated :: CExpr -> Maybe CExpr
allocated = \case
    CCast _ e _ -> allocated e
    CCall (CVar f _) args _ -> case (identToString f, args) of
        ("alloca", [n])        -> Just n
        ("malloc", [n])        -> Just n
        ("mem_balloc", [_, n]) -> Just n
        _                      -> Nothing
    _ -> Nothing

-- | The functions the check looks at: those that declare an array whose size
-- isn't a constant, or allocate memory.
looksAt :: FunDef -> Bool
looksAt (FunDef _ body _) = any sized (localDecls body)
  where
    sized = \case
        CDecl _ ds _ -> or
            [ any variable derived || maybe False allocates ini
            | (Just (CDeclr _ derived _ _ _), ini, _) <- ds ]
        _ -> False
    variable = \case
        CArrDeclr _ (CArrSize _ CConst{}) _ -> False
        CArrDeclr _ (CArrSize _ _) _        -> True
        _                                   -> False
    allocates = \case
        CInitExpr e _ -> isJust (allocated e)
        _             -> False

-- | The byte buffers among a function's locals that always point at the
-- memory they were made with, the expressions that give their sizes, and
-- whether those expressions keep their values.
buffers :: FunDef -> [IdentDecl] -> [(String, (CExpr, Bool))]
buffers (FunDef _ body _) locals =
    [ (name, (size, all (`Set.notMember` assigned) (names size)))
    | ObjectDef (ObjDef (VarDecl (VarName i _) _ ty) ini _) <- locals
    , let name = identToString i
    , name `Set.notMember` assigned
    , Just size <- [sizeOf (canonicalType ty) ini]
    ]
  where
    sizeOf ty ini = case ty of
        ArrayType el (ArraySize _ e) _ _ | isByte el            -> Just e
        PtrType el _ _ | isByte el, Just (CInitExpr e _) <- ini -> allocated e
        _                                                       -> Nothing
    isByte el = maybe False ((== 8) . tyBits) (intType el)

    assigned = Set.fromList [ identToString i | e <- concatMap everyExpr (statExprs body), CVar i _ <- written e ]
    everyExpr e = e : concatMap everyExpr (exprChildren e)
    names e = [ identToString i | CVar i _ <- everyExpr e ]
    written = \case
        CAssign _ l _ _ -> [l]
        CUnary op l _ | op `elem` [CPreIncOp, CPreDecOp, CPostIncOp, CPostDecOp, CAdrOp] -> [l]
        _ -> []

-- | An access to a buffer: the buffer, the terms and constant whose sum is how
-- far into it the access reaches, and what makes it.
data Access = Access
    { accBuffer :: String
    , accTerms  :: [Term]
    , accConst  :: Integer
    , accWhat   :: Doc AnsiStyle
    }

-- | The buffer accesses a term makes.
accesses :: Term -> [Access]
accesses t = case termKind t of
    TCall f args | CVar g _ <- termExpr f -> case (identToString g, args) of
        (fn, [d, s, n]) | fn `elem` ["memcpy", "memmove"] -> mapMaybe (call fn n) [d, s]
        (fn@"memset", [d, _, n]) -> mapMaybe (call fn n) [d]
        _ -> []
    _ | Just (b, ts, k) <- element t -> [Access b ts (k + 1) (exprDoc (termExpr t))]
    _ -> []
  where
    call fn n p = case termKind p of
        TCast a -> call fn n a
        _       -> (\(b, ts, k) -> Access b (ts ++ [n]) k (backticks (pretty fn))) <$> pointsInto p

-- | The buffer a pointer points into, and the terms and constant it adds.
pointsInto :: Term -> Maybe (String, [Term], Integer)
pointsInto t = case termKind t of
    TBin CAddOp a k | isJust (termTy k) -> (\(b, ts, c) -> (b, ts ++ [k], c)) <$> pointsInto a
    TUnary CAdrOp x -> element x
    _ | CVar i _ <- termExpr t -> Just (identToString i, [], 0)
    _ -> Nothing

-- | The buffer an element is in, and the terms and constant that index it. An
-- element at a constant index is a path the analysis tracks.
element :: Term -> Maybe (String, [Term], Integer)
element t = case (termKind t, termExpr t) of
    (TIndex a i _, _) -> (\(b, ts, c) -> (b, ts ++ [i], c)) <$> pointsInto a
    (TLoad _, CIndex (CVar b _) (CConst (CIntConst k _)) _) -> Just (identToString b, [], getCInteger k)
    _ -> Nothing

-- | How many bytes into its buffer an access can reach.
reach :: Facts -> Access -> Maybe Integer
reach facts acc = (+ accConst acc) . sum <$> mapM highest (accTerms acc)
  where
    highest x = eval facts x >>= \case
        [] -> Nothing
        rs -> Just (maximum (map hi rs))

-- | Whether an access stays within a size, whatever their values.
fits :: Facts -> Access -> Term -> Bool
fits facts acc = boundedBy facts (accTerms acc) (accConst acc)

-- | A size without its factor of one, like @sizeof(uint8_t)@.
inBytes :: Term -> Term
inBytes t = case termKind t of
    TBin CMulOp a b | TConst 1 <- termKind b -> inBytes a
    _                                        -> t

bytesDoc :: Integer -> Doc a
bytesDoc 1 = "1 byte"
bytesDoc n = pretty n <+> "bytes"

analyse :: GlobalDecls -> Trav Env ()
analyse _ = do
    env <- getUserState
    functions <- analysedFunctions looksAt
    forM_ functions $ \(fd@(FunDef (VarDecl self _ _) _ _), nodes) -> do
        let locals = case self of
                VarName i _ -> Map.findWithDefault [] (identToString i) (localsOf env)
                NoName      -> []
            bufs = Map.fromList (buffers fd locals)
            sizeNames = Map.fromList [ (n, (b, kept)) | (b, (e, kept)) <- Map.toList bufs, Just n <- [nameOfNode (annotation e)] ]
            -- Each buffer's size, the values it has when the buffer is made,
            -- and whether it keeps them.
            sizes = Map.fromList
                [ (b, (inBytes s, rs, kept))
                | (_, facts, terms) <- nodes, s <- concatMap subTerms terms
                , Just (b, kept) <- [nameOfNode (annotation (termExpr s)) >>= (`Map.lookup` sizeNames)]
                , Just rs <- [eval facts (inBytes s)] ]
            problems =
                [ (posOfNode (annotation (termExpr t)), accBuffer acc, report t acc (termExpr size) r site end)
                | (_, facts, terms) <- nodes, t <- concatMap subTerms terms, acc <- accesses t
                , Just (size, rs, kept) <- [Map.lookup (accBuffer acc) sizes]
                , not (kept && fits facts acc size)
                , Just end <- [reach facts acc]
                , (r, site) : _ <- [[ (r, site) | r <- rs, Just site <- [wrapSite (origin r)], lo r < end ]] ]
        -- One error for each buffer, at the first access past its size.
        sequence_ . Map.elems $ Map.fromListWith (\_ old -> old)
            [ (b, act) | (_, b, act) <- sortOn (\(pos, _, _) -> pos) problems ]
  where
    report t acc size r site end = recordWrap t site $
        exprDoc size <+> "can wrap around to" <+> valuesDoc r
        <> ", so" <+> backticks (pretty (accBuffer acc)) <+> "can be smaller than the"
        <+> bytesDoc end <+> accWhat acc <+> "reaches"


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("buffer-size", Text.unlines
    [ "Checks for byte buffers whose size can wrap around and that are then accessed"
    , "past the wrapped size: arrays of variable length, and memory from `alloca`,"
    , "`malloc` or `mem_balloc`, accessed by `memcpy`, `memmove`, `memset` or indexing."
    , ""
    , "**Reason:** `const uint16_t size = length + HEADER_SIZE;` truncates to a small"
    , "value when `length` is close to 65536, and copying `length` bytes into a"
    , "buffer of that size writes past its end. Compute sizes in a type that can hold"
    , "them, and limit them before making the buffer."
    ]))
