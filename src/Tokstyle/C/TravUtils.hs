module Tokstyle.C.TravUtils where

import           Language.C.Analysis.TravMonad (MonadTrav, throwTravError)
import           Language.C.Data.Error         (userErr)
import           Prettyprinter                 (Doc, pretty)


getJust :: MonadTrav m => String -> Maybe a -> m a
getJust _ (Just ok) = return ok
getJust failMsg Nothing =
    throwTravError $ userErr failMsg

backticks :: Doc ann -> Doc ann
backticks d = pretty '`' <> d <> pretty '`'

dquotes :: Doc ann -> Doc ann
dquotes d = pretty '"' <> d <> pretty '"'
