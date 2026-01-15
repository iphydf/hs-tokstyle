{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Strict         #-}
module Tokstyle.C.Env where

import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)
import           Language.C.Analysis.SemRep    (GlobalDecls, Type)
import           Language.C.Analysis.TravMonad (Trav, getUserState,
                                                modifyUserState)
import           Language.C.Data.Ident         (Ident, SUERef)
import           Language.C.Data.Node          (NodeInfo (..), nodeInfo,
                                                posOfNode)
import           Language.C.Data.Position      (Position, posColumn, posFile,
                                                posRow)
import           Prettyprinter                 (Doc)
import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Tokstyle.C.TravUtils          (getJust)

data DiagnosticLevel = ErrorLevel | WarningLevel | NoteLevel | HelpLevel
    deriving (Show, Eq)

data DiagnosticSpan = DiagnosticSpan
    { spanPos    :: Position
    , spanLen    :: Int
    , spanLabels :: [Doc AnsiStyle]
    }

data LinterError = LinterError
    { linterErrorPos    :: Position
    , linterErrorLen    :: Int
    , linterErrorLevel  :: DiagnosticLevel
    , linterErrorMsg    :: Doc AnsiStyle
    , linterErrorFlag   :: Maybe Text
    , linterErrorSpans  :: [DiagnosticSpan]
    , linterErrorFooter :: [(DiagnosticLevel, Doc AnsiStyle)]
    }

data Env = Env
    { ctx           :: [String]
    , retTy         :: Maybe Type
    , params        :: [Ident]
    , inferredTypes :: Map Ident Type
    , mainTypedefs  :: Map SUERef Ident
    , globalDecls   :: Maybe GlobalDecls
    , linterErrors  :: [LinterError]
    , currentFlag   :: Maybe Text
    }

defaultEnv :: Env
defaultEnv = Env ["file"] Nothing [] Map.empty Map.empty Nothing [] Nothing

bracketUserState :: (Env -> Env) -> Trav Env a -> Trav Env a
bracketUserState f act = do
    s <- getUserState
    modifyUserState f
    r <- act
    modifyUserState $ \s' -> s{linterErrors = linterErrors s'}
    return r


getCtx :: Trav Env [String]
getCtx = ctx <$> getUserState

pushCtx :: String -> Trav Env ()
pushCtx s = modifyUserState $ \env@Env{ctx} -> env{ctx = s:ctx}

popCtx :: Trav Env ()
popCtx = modifyUserState $ \env@Env{ctx} -> env{ctx = drop 1 ctx}


getRetTy :: Trav Env Type
getRetTy = getUserState >>= getJust "not in function context" . retTy

setRetTy :: Type -> Trav Env ()
setRetTy t = modifyUserState $ \env -> env{retTy = Just t}

unsetRetTy :: Trav Env ()
unsetRetTy = modifyUserState $ \env -> env{retTy = Nothing}

recordLinterError :: NodeInfo -> Doc AnsiStyle -> Trav Env ()
recordLinterError info doc = modifyUserState $ \env ->
    let (pos, len) = posAndLen info
    in env{linterErrors = LinterError pos len ErrorLevel doc (currentFlag env) [] [] : linterErrors env}

recordRichError :: NodeInfo -> DiagnosticLevel -> Doc AnsiStyle -> [DiagnosticSpan] -> [(DiagnosticLevel, Doc AnsiStyle)] -> Trav Env ()
recordRichError info level msg spans footer = modifyUserState $ \env ->
    let (pos, len) = posAndLen info
    in env{linterErrors = LinterError pos len level msg (currentFlag env) spans footer : linterErrors env}

posAndLen :: NodeInfo -> (Position, Int)
posAndLen info = case nodeInfo info of
    NodeInfo pos (lastPos, lastLen) _ -> (pos, calcLen pos lastPos lastLen)
    OnlyPos pos (lastPos, lastLen)    -> (pos, calcLen pos lastPos lastLen)
  where
    calcLen pos lastPos lastLen =
        if posFile pos == posFile lastPos && posRow pos == posRow lastPos
        then (posColumn lastPos + lastLen) - posColumn pos
        else lastLen
