{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.SizeArg (analyse) where

import           Data.Functor.Identity           (Identity)
import qualified Data.List                       as List
import qualified Data.Map                        as Map
import           Language.C.Analysis.AstAnalysis (ExprSide (..), defaultMD,
                                                  tExpr)
import           Language.C.Analysis.ConstEval   (constEval, intValue)
import           Language.C.Analysis.SemError    (invalidAST)
import           Language.C.Analysis.SemRep      (GlobalDecls, ParamDecl (..),
                                                  Type (..))
import           Language.C.Analysis.TravMonad   (Trav, TravT, catchTravError,
                                                  throwTravError)
import           Language.C.Analysis.TypeUtils   (canonicalType)
import           Language.C.Data.Ident           (Ident (..))
import qualified Language.C.Pretty               as C
import           Language.C.Syntax.AST           (CExpr, CExpression (..),
                                                  annotation)
import           Prettyprinter                   (pretty)
import           Tokstyle.C.Env                  (Env, recordLinterError)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)


checkArraySizes :: Ident -> [(ParamDecl, CExpr, Type)] -> Trav Env ()
checkArraySizes funId ((_, _, arrTy@(ArrayTypeSize arrSize)):(ParamName sizeParam, sizeArg, _):args)
    | any (`List.isInfixOf` sizeParam) ["size", "len"] =
        -- Ignore any name lookup errors here. VLAs have locally defined
        -- array sizes, but we don't check VLAs.
        catchTravError (do
            arrSizeVal <- intValue <$> constEval defaultMD Map.empty arrSize
            sizeArgVal <- intValue <$> constEval defaultMD Map.empty sizeArg
            case (arrSizeVal, sizeArgVal) of
                (Just arrSizeConst, Just sizeArgConst) | arrSizeConst < sizeArgConst ->
                    recordLinterError (annotation sizeArg) $
                        "size parameter `" <> pretty sizeParam <> "` is passed constant value `"
                        <> pretty (show (C.pretty sizeArg)) <> "` (= " <> pretty sizeArgConst <> "),\n"
                        <> "  which is greater than the array size of `" <> pretty (show (C.pretty arrTy)) <> "`,\n"
                        <> "  potentially causing buffer overrun in `" <> pretty (show (C.pretty funId)) <> "`"
                _ -> return ()  -- not constant, or array size greater than size arg
            checkArraySizes funId args
        ) $ const $ return ()

checkArraySizes funId (_:xs) = checkArraySizes funId xs
checkArraySizes _ [] = return ()


linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        CCall fun@(CVar funId _) args _ ->
            tExpr [] RValue fun >>= \case
                FunPtrParams params -> do
                    tys <- mapM (fmap canonicalType . tExpr [] RValue) args
                    checkArraySizes funId (zip3 params args tys)
                    act
                x -> throwTravError $ invalidAST (annotation node) $ show x

        _ -> act
    }


analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter
