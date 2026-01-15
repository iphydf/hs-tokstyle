{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Main (main) where

import           Control.Monad                 (unless)
import qualified Control.Monad.Parallel        as Par
import           Data.List                     (find, isPrefixOf, partition)
import qualified Data.Maybe                    as Maybe
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import qualified Data.Text.Lazy.IO             as TLIO
import           Language.C                    (parseCFile)
import           Language.C.System.GCC         (newGCC)
import           Prettyprinter                 (Doc, defaultLayoutOptions,
                                                layoutSmart, pretty)
import           Prettyprinter.Render.Terminal (AnsiStyle, renderIO)
import           Prettyprinter.Render.Text     (renderLazy)
import           System.Environment            (getArgs)
import           System.Exit                   (ExitCode (..), exitWith)
import           System.IO                     (hSetEncoding, stderr, stdout,
                                                utf8)
import qualified Tokstyle.C.Linter             as Linter
import           Tokstyle.C.Linter             (allWarnings, analyse)


defaultCppOpts :: String -> [String]
defaultCppOpts sysInclude =
    [ "-DCMP_NO_FLOAT"      -- avoid float->char* casts
    , "-DWORDS_BIGENDIAN=0" -- avoid casting in is_bigendian()
    , "-nostdinc"           -- we have our own stdlib headers
    , "-undef"              -- no __linux__
    , "-I" <> sysInclude
    , "-I" <> sysInclude <> "/opus"
    , "-Wno-unused-command-line-argument"
    ]


processFile :: String -> String -> [Text] -> [String] -> FilePath -> IO (Bool, [Doc AnsiStyle])
processFile cc sysInclude flags cppOpts file = do
    result <- parseCFile (newGCC cc) Nothing (defaultCppOpts sysInclude ++ cppOpts) file
    case result of
        Left err -> return (False, [pretty file <> ": Parse Error: " <> pretty (show err)])
        Right tu -> do
            errs <- Linter.renderIO $ analyse flags tu
            case errs of
                [] -> return (True, [])
                _  -> return (False, errs)


main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    args <- getArgs
    let (opts, rest) = partition (isPrefixOf "--") args
    let (restOpts, files) = partition (isPrefixOf "-") rest
    let (linterOpts, cppOpts) = partition (isPrefixOf "-W") restOpts
    let flags = processFlags linterOpts
    let cc = Maybe.fromMaybe "clang" $ getFlag "--cc=" opts
    let sysInclude = Maybe.fromMaybe "/src/workspace/hs-tokstyle/include" $ getFlag "--include=" opts
    result <- Par.mapM (processFile cc sysInclude flags cppOpts) files
    mapM_ (mapM_ render . snd) result
    unless (all fst result) $ exitWith (ExitFailure 1)
  where
    getFlag flag = fmap (drop $ length flag) . find (isPrefixOf flag)

    render doc = renderIO stderr (layoutSmart defaultLayoutOptions doc) >> Text.hPutStrLn stderr ""

    processFlags :: [String] -> [Text]
    processFlags flags =
        let stripped = map (drop 2) flags
            initial = if any (not . ("no-" `isPrefixOf`)) stripped then [] else allWarnings
        in foldr processFlag initial . reverse $ stripped

    processFlag :: String -> [Text] -> [Text]
    processFlag ('n':'o':'-':flag) = filter (/= Text.pack flag)
    processFlag flag               = (Text.pack flag :)
