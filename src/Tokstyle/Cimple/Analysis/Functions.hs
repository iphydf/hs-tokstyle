{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
-- | The functions of a program, what their returns say about success, and
-- the parts of their bodies that analyses of them look at.
module Tokstyle.Cimple.Analysis.Functions
    ( Function (..)
    , Shape (..)
    , Outcome (..)
    , definitions
    , callees
    , returnOutcomes
    , branchOn
    , calleeLexeme
    , universe
    , strip
    , unFix'
    , intLit
    , constName
    , isEnumConst
    , localDecls
    , assignments
    , nodeExprs
    , rootOf
    , throughPointer
    , render
    ) where

import           Data.Fix                     (Fix (..))
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (mapMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Cimple              (BinaryOp (..), Lexeme (..),
                                               NodeF (..), UnaryOp (..))
import qualified Language.Cimple              as C
import           Language.Cimple.Pretty       (showNodePlain)
import           Prettyprinter                (Doc, pretty)
import           Tokstyle.Analysis.AccessPath (AccessPath (..))
import           Tokstyle.Cimple.Analysis.CFG (Node (..), NodeKind (..),
                                               getFuncName)


-- | How a function's callers can tell it succeeded.
data Shape
    = BoolShape
    -- ^ @true@ on success, @false@ on failure.
    | ZeroShape
    -- ^ 0 on success, a negative number on failure.
    | IdShape
    -- ^ A negative number on failure, anything else on success.
    | PtrShape
    -- ^ @nullptr@ on failure.
    | NoShape

data Outcome = Success | Failure
    deriving (Eq, Ord, Show, Enum, Bounded)

data Function = Function
    { fnFile   :: FilePath
    , fnName   :: Text
    , fnParams :: [Text]
    , fnShape  :: Shape
    , fnDefn   :: C.Node (Lexeme Text)
    }

universe :: C.Node (Lexeme Text) -> [C.Node (Lexeme Text)]
universe node@(Fix n) = node : concatMap universe (foldr (:) [] n)

lexemeText :: Lexeme Text -> Text
lexemeText (L _ _ t) = t

strip :: C.Node (Lexeme Text) -> C.Node (Lexeme Text)
strip = \case
    Fix (ParenExpr e) -> strip e
    e                 -> e

-- | The value of an integer literal, as in @0@ or @-1@.
intLit :: C.Node (Lexeme Text) -> Maybe Integer
intLit e = case unFix' (strip e) of
    LiteralExpr C.Int (L _ _ t)                    -> readInt t
    UnaryExpr UopMinus (Fix (LiteralExpr C.Int l)) -> negate <$> readInt (lexemeText l)
    _                                              -> Nothing
  where
    readInt t = case reads (Text.unpack t) of
        [(n, "")] -> Just n
        _         -> Nothing

unFix' :: C.Node (Lexeme Text) -> NodeF (Lexeme Text) (C.Node (Lexeme Text))
unFix' (Fix n) = n

-- | The name a literal constant has, as in @true@, @nullptr@ or @TOX_OK@.
constName :: C.Node (Lexeme Text) -> Maybe Text
constName e = case unFix' (strip e) of
    LiteralExpr _ (L _ _ t) | Text.all (\c -> c == '_' || c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9']) t
                            , not (Text.null t) -> Just t
    _ -> Nothing

isEnumConst :: Text -> Bool
isEnumConst t = Text.all (\c -> c == '_' || c `elem` ['A'..'Z'] || c `elem` ['0'..'9']) t
    && Text.any (`elem` ['A'..'Z']) t

shapeOf :: C.Node (Lexeme Text) -> C.Node (Lexeme Text) -> Shape
shapeOf retType body
    | "bool" `Text.isInfixOf` ty = BoolShape
    | "*" `Text.isInfixOf` ty = PtrShape
    | any (`Text.isInfixOf` ty) ["int", "Index", "Number"] = intShape
    | otherwise = NoShape
  where
    ty = showNodePlain retType
    returned = [ e | Fix (Return (Just e)) <- universe body ]
    lits = map intLit returned
    intShape
        | all (maybe False (<= 0)) lits && Just 0 `elem` lits = ZeroShape
        | any (maybe False (< 0)) lits = IdShape
        | otherwise = NoShape

-- | The outcome a return statement reports.
returnOutcomes :: Shape -> Maybe (C.Node (Lexeme Text)) -> [Outcome]
returnOutcomes shape ret = case (shape, ret) of
    (BoolShape, Just e) | constName e == Just "true"  -> [Success]
                        | constName e == Just "false" -> [Failure]
    (ZeroShape, Just e) | intLit e == Just 0               -> [Success]
                        | maybe False (< 0) (intLit e)     -> [Failure]
    (IdShape, Just e)   | maybe False (< 0) (intLit e)     -> [Failure]
                        | otherwise                        -> [Success]
    (PtrShape, Just e)  | constName e `elem` [Just "nullptr", Just "NULL"] -> [Failure]
                        | otherwise                        -> [Success]
    (NoShape, _) -> [Success]
    _ -> [Success, Failure]

-- | For a branch on what a call returned: the call, and the outcome it had on
-- the true branch.
branchOn :: Map Text Shape -> C.Node (Lexeme Text) -> Maybe (Lexeme Text, Outcome)
branchOn shapeMap cond = case unFix' (strip cond) of
    UnaryExpr UopNot e -> fmap flipOutcome <$> branchOn shapeMap e
    BinaryExpr l op r
        | Just (f, s) <- call l, Just n <- intLit r -> compareInt f s op n
        | Just (f, PtrShape) <- call l, constName r `elem` [Just "nullptr", Just "NULL"] -> case op of
            BopEq -> Just (f, Failure)
            BopNe -> Just (f, Success)
            _     -> Nothing
    _ | Just (f, s) <- call cond -> case s of
        BoolShape -> Just (f, Success)
        PtrShape  -> Just (f, Success)
        ZeroShape -> Just (f, Failure)
        _         -> Nothing
    _ -> Nothing
  where
    call e = case unFix' (strip e) of
        FunctionCall callee _ | Just name <- getFuncName callee, Just s <- Map.lookup name shapeMap ->
            Just (calleeLexeme callee, s)
        _ -> Nothing
    flipOutcome Success = Failure
    flipOutcome Failure = Success
    compareInt f s op n = case (s, op, n) of
        (ZeroShape, BopNe, 0) -> Just (f, Failure)
        (ZeroShape, BopEq, 0) -> Just (f, Success)
        (BoolShape, BopNe, 0) -> Just (f, Success)
        (BoolShape, BopEq, 0) -> Just (f, Failure)
        (_, BopLt, 0)         -> Just (f, Failure)
        (_, BopGe, 0)         -> Just (f, Success)
        (_, BopEq, -1)        -> Just (f, Failure)
        (_, BopNe, -1)        -> Just (f, Success)
        _                     -> Nothing

calleeLexeme :: C.Node (Lexeme Text) -> Lexeme Text
calleeLexeme e = case unFix' (strip e) of
    VarExpr l       -> l
    LiteralExpr _ l -> l
    _               -> L (C.AlexPn 0 0 0) C.IdVar ""

-- | The function definitions in a list of top-level nodes, with their files.
definitions :: [(FilePath, [C.Node (Lexeme Text)])] -> [Function]
definitions tus = [ f | (file, nodes) <- tus, node <- nodes, f <- go file node ]
  where
    go file node@(Fix n) = case n of
        FunctionDefn _ (Fix (FunctionPrototype ret (L _ _ name) params)) body ->
            [Function file name (mapMaybe paramName params) (shapeOf ret body) node]
        _ -> concatMap (go file) (foldr (:) [] n)
    paramName = \case
        Fix (VarDecl _ (L _ _ p) _) -> Just p
        Fix (NonNullParam p)        -> paramName p
        Fix (NullableParam p)       -> paramName p
        _                           -> Nothing

-- | The locals of a function, with what they are initialised with.
localDecls :: C.Node (Lexeme Text) -> [(String, Maybe (C.Node (Lexeme Text)))]
localDecls defn = [ (Text.unpack v, ini) | Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) ini) <- universe defn ]

-- | What each local is assigned after its declaration.
assignments :: C.Node (Lexeme Text) -> Map String [C.Node (Lexeme Text)]
assignments defn = Map.fromListWith (++)
    [ (Text.unpack v, [r]) | Fix (AssignExpr (Fix (VarExpr (L _ _ v))) _ r) <- universe defn ]

rootOf :: AccessPath -> Maybe String
rootOf = \case
    PathVar v     -> Just v
    PathField p _ -> rootOf p
    PathIndex p _ -> rootOf p
    PathDeref p   -> rootOf p
    _             -> Nothing

-- | Whether a path goes through a pointer, so that what it names outlives
-- the function.
throughPointer :: AccessPath -> Bool
throughPointer = \case
    PathDeref{}   -> True
    PathField p _ -> throughPointer p
    PathIndex p _ -> throughPointer p
    _             -> False

render :: AccessPath -> Doc ann
render = pretty . go
  where
    go = \case
        PathVar v                 -> v
        PathField (PathDeref p) m -> go p <> "->" <> m
        PathField p m             -> go p <> "." <> m
        PathIndex p i             -> go p <> "[" <> i <> "]"
        PathDeref p               -> "*" <> go p
        p                         -> show p

-- | The expressions a node of the graph evaluates.
nodeExprs :: Node -> [C.Node (Lexeme Text)]
nodeExprs (Node _ kind) = case kind of
    BranchNode e -> [e]
    StmtNode (Fix s) -> case s of
        ExprStmt e             -> [e]
        Return (Just e)        -> [e]
        VarDeclStmt _ (Just e) -> [e]
        _ | isExpr s           -> [Fix s]
        _                      -> []
    _ -> []
  where
    isExpr = \case
        FunctionCall{} -> True
        AssignExpr{}   -> True
        UnaryExpr{}    -> True
        _              -> False

-- | The functions each function calls.
callees :: Function -> [Text]
callees fn = [ name | Fix (FunctionCall callee _) <- universe (fnDefn fn), Just name <- [getFuncName callee] ]
