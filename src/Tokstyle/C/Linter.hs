{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter
    ( analyse
    , allWarnings
    , renderIO
    , renderPure
    , LinterError(..)
    , DiagnosticLevel(..)
    , Doc
    , AnsiStyle
    ) where

import qualified Data.ByteString                  as BS
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import qualified Data.Text.IO                     as Text
import           Language.C.Analysis.AstAnalysis  (analyseAST)
import           Language.C.Analysis.SemRep       (GlobalDecls)
import           Language.C.Analysis.TravMonad    (CLanguage (..), Trav,
                                                   TravOptions (..),
                                                   modifyOptions,
                                                   modifyUserState, runTrav,
                                                   userState)
import           Language.C.Data.Error            (CError, errorPos)
import           Language.C.Data.Node             (NodeInfo (..), nodeInfo)
import           Language.C.Data.Position         (Position (..), isSourcePos,
                                                   posColumn, posFile, posOf,
                                                   posRow)
import qualified Language.C.Pretty                as C
import           Language.C.Syntax.AST            (CTranslUnit)
import           Prettyprinter                    (Doc, align, annotate, line,
                                                   nest, pretty, vsep, (<+>))
import qualified Prettyprinter                    as PP
import           Prettyprinter.Render.Terminal    (AnsiStyle, Color (..), bold,
                                                   color, colorDull)
import qualified Tokstyle.C.Env                   as Env
import           Tokstyle.C.Env                   (DiagnosticLevel (..),
                                                   DiagnosticSpan (..), Env,
                                                   LinterError (..), defaultEnv,
                                                   linterErrors)

import qualified Tokstyle.C.Linter.BoolConversion as BoolConversion
import qualified Tokstyle.C.Linter.CallbackData   as CallbackData
import qualified Tokstyle.C.Linter.CallbackParams as CallbackParams
import qualified Tokstyle.C.Linter.Cast           as Cast
import qualified Tokstyle.C.Linter.Conversion     as Conversion
import qualified Tokstyle.C.Linter.Memset         as Memset
import qualified Tokstyle.C.Linter.SizeArg        as SizeArg
import qualified Tokstyle.C.Linter.Sizeof         as Sizeof
import qualified Tokstyle.C.Linter.StrictTypedef  as StrictTypedef
import qualified Tokstyle.C.Linter.VoidCall       as VoidCall


linters :: [(Text, GlobalDecls -> Trav Env ())]
linters =
    [ ("bool-conversion"    , BoolConversion.analyse   )
    , ("callback-data"      , CallbackData.analyse     )
    , ("callback-params"    , CallbackParams.analyse   )
    , ("cast"               , Cast.analyse             )
    , ("conversion"         , Conversion.analyse       )
    , ("memset"             , Memset.analyse           )
    , ("size-arg"           , SizeArg.analyse          )
    , ("sizeof"             , Sizeof.analyse           )
    , ("strict-typedef"     , StrictTypedef.analyse    )
    , ("void-call"          , VoidCall.analyse         )
    ]
runLinters :: [Text] -> GlobalDecls -> Trav Env ()
runLinters flags tu =
    mapM_ (\(flag, f) -> do
        modifyUserState $ \env -> env{Env.currentFlag = Just flag}
        f tu
        modifyUserState $ \env -> env{Env.currentFlag = Nothing}
    ) . filter ((`elem` flags) . fst) $ linters


analyse :: [Text] -> CTranslUnit -> [LinterError]
analyse enabled tu =
    case analysis of
        Left errs        -> map toLinterError errs
        Right (_, state) -> reverse . linterErrors $ userState state
  where
    analysis = runTrav defaultEnv $ do
        modifyOptions (\opts -> opts { language = GNU99 })
        decls <- analyseAST tu
        runLinters enabled decls

    toLinterError :: CError -> LinterError
    toLinterError err = LinterError (errorPos err) 1 ErrorLevel (pretty (show err)) Nothing [] []


renderIO :: [LinterError] -> IO [Doc AnsiStyle]
renderIO errors = do
    let files = Map.keys . Map.fromList $ [ (posFile (linterErrorPos e), ()) | e <- errors, isSourcePos (linterErrorPos e) ]
    cache <- Map.fromList <$> mapM (\f -> do
        ls <- Text.lines . Text.decodeUtf8 <$> BS.readFile f
        return (f, ls)) files
    return $ renderPure cache errors


renderPure :: Map FilePath [Text] -> [LinterError] -> [Doc AnsiStyle]
renderPure cache = map (formatRichError cache)
  where
    formatRichError cache' (LinterError p len level msg flag spans footers) =
        vsep $
        [ let msgLines = map (pretty . Text.stripEnd) $ Text.lines (Text.pack (show (PP.unAnnotate msg)))
          in case msgLines of
            (l:ls) -> header <+> align (vsep ((case flag of
                Just f -> l <+> annotate (colorDull White) ("[-W" <> pretty f <> "]")
                Nothing -> l) : ls))
            []     -> case flag of
                Just f -> header <+> annotate (colorDull White) ("[-W" <> pretty f <> "]")
                Nothing -> header
        ] ++
        [ annotate (colorDull White) "   -->" <+> annotate (bold <> color White) (pretty (posFile p) <> ":" <> pretty (posRow p) <> ":" <> pretty (posColumn p))
        | isSourcePos p ] ++
        (if null snippet then [] else [annotate (colorDull White) "    |"] ++ snippet ++ [annotate (colorDull White) "    |"]) ++
        map formatFooter footers
      where
                header = case level of
                    ErrorLevel   -> annotate (color Red <> bold) "error:"
                    WarningLevel -> annotate (color Yellow <> bold) "warning:"
                    NoteLevel    -> annotate (color Cyan <> bold) "note:"
                    HelpLevel    -> annotate (color Green <> bold) "help:"

                spansToShow = if null spans && isSourcePos p then [DiagnosticSpan p len []] else spans
                snippet = renderSnippet spansToShow (if isSourcePos p then Map.lookup (posFile p) cache' else Nothing)

                formatFooter (l, d) =
                    let pref = case l of
                            ErrorLevel   -> annotate (color Red <> bold) "   = error:"
                            WarningLevel -> annotate (color Yellow <> bold) "   = warning:"
                            NoteLevel    -> annotate (color Cyan <> bold) "   = note:"
                            HelpLevel    -> annotate (color Green <> bold) "   = help:"
                    in pref <+> align d
    renderSnippet spans' mFileLines =
        case spans' of
            (DiagnosticSpan sp len labels : _) ->
                let row = posRow sp
                    col = posColumn sp
                    padding = Text.replicate (max 0 (col - 1)) " "
                    underline = annotate (color Red <> bold) (pretty padding <> pretty (Text.replicate (max 1 len) "^"))
                    labelDocs = map (\l -> line <> "    |" <+> pretty padding <> align l) labels
                    fullLabel = if null labels
                                then mempty
                                else line <> "    |" <+> pretty padding <> "|" <> mconcat labelDocs
                in case mFileLines of
                    Just fileLines | row > 0 && row <= length fileLines ->
                        let lineText = fileLines !! (row - 1)
                            gutter = pretty (show row ++ replicate (max 0 (4 - length (show row))) ' ') <> "|"
                        in [ gutter <+> pretty lineText
                           , "    |" <+> underline <> fullLabel
                           ]
                    _ -> [ "    |" <+> underline <> fullLabel ]
            _ -> []


allWarnings :: [Text]
allWarnings = map fst linters
