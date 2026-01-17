{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Arrow                 (second)
import           Control.Parallel.Strategies   (parMap, rpar)
import qualified Data.ByteString               as BS
import           Data.List                     (isPrefixOf)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Text.IO                  as Text
import           Data.Time.Clock               (UTCTime, diffUTCTime,
                                                getCurrentTime)
import           Language.Cimple               (Lexeme, Node)
import           Language.Cimple.Diagnostics   (CimplePos, Diagnostic (..),
                                                IsPosition (..), renderPure)
import           Language.Cimple.IO            (parseProgram)
import qualified Language.Cimple.Program       as Program
import           Options.Applicative
import           Prettyprinter                 (Doc)
import           Prettyprinter.Render.Terminal (AnsiStyle, putDoc)
import           System.IO                     (hPutStrLn, stderr)
import           Tokstyle.Linter               (allWarnings, analyseGlobal,
                                                analyseLocal, markdown)


processAst :: [Text] -> (UTCTime, [(FilePath, [Node (Lexeme Text)])]) -> IO ()
processAst ignore (start, tus) = do
    let diags = concat $ analyseGlobal ignore tus : parMap rpar (analyseLocal ignore) tus
    report diags
    end <- getCurrentTime
    hPutStrLn stderr $ "Linting: " <> show (diffUTCTime end start)
  where
    report [] = return ()
    report diags = do
        let files = Map.keys . Map.fromList $ [ (posFile (diagPos e), ()) | e <- diags ]
        cache <- Map.fromList <$> mapM (\f -> do
            ls <- Text.lines . Text.decodeUtf8 <$> BS.readFile f
            return (f, ls)) files
        mapM_ (\d -> putDoc d >> putStrLn "") (renderPure cache diags)
        fail "tokstyle violations detected"


data Options = Options
    { optMarkdown :: Bool
    , optWarnings :: [Text]
    , optFiles    :: [FilePath]
    }


parseOptions :: [Text] -> Parser Options
parseOptions allW = Options
    <$> switch (long "markdown" <> help "Output documentation in Markdown format")
    <*> (processFlags . map Text.pack <$> many (strOption (short 'W' <> metavar "WARNING" <> help "Enable/disable warnings")))
    <*> many (argument str (metavar "FILES..."))
  where
    processFlags flags =
        let stripped = map Text.unpack flags
            initial = if any (not . ("no-" `isPrefixOf`)) stripped then [] else allW
        in foldl (flip processFlag) initial stripped

    processFlag ('n':'o':'-':f) = filter (/= Text.pack f)
    processFlag f               = (Text.pack f :)


main :: IO ()
main = do
    opts <- execParser $ info (parseOptions allWarnings <**> helper) fullDesc
    if optMarkdown opts
        then Text.putStr markdown
        else do
            start <- getCurrentTime
            hPutStrLn stderr $ "Parsing " <> show (length (optFiles opts)) <> " files..."
            parseProgram (optFiles opts) >>= getRight start >>= (processAst (optWarnings opts) . second Program.toList)


getRight :: UTCTime -> Either String a -> IO (UTCTime, a)
getRight _ (Left err) = putStrLn err >> fail "aborting after parse error"
getRight start (Right ok) = do
    end <- getCurrentTime
    hPutStrLn stderr $ "Parsing: " <> show (diffUTCTime end start)
    return (end, ok)
