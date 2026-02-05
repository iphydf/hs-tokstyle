module Tokstyle.Analysis.DataflowSpec (spec) where
import           Data.Text                  (unlines)
import           Prelude                    hiding (unlines)

import           Test.Hspec

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Tokstyle.Analysis.Dataflow

spec :: Spec
spec = describe "Dataflow Solver" $ do
    it "reaches a fixed point for a simple increment problem" $ do
        -- Graph: 0 -> 1 -> 2
        --             ^----
        -- Node 0: Entry, Node 1: Add 1, Node 2: If < 5 GOTO 1
        let cfg :: Map Int [(MyEdge, Int)]
            cfg = Map.fromList
                [ (0 :: Int, [(MyTrue, 1 :: Int)])
                , (1 :: Int, [(MyTrue, 2 :: Int)])
                , (2 :: Int, [(MyTrue, 1 :: Int), (MyFalse, 3 :: Int)])
                , (3 :: Int, [])
                ]

            problem :: Dataflow Int MyEdge Int
            problem = Dataflow
                { transfer = \node val -> if (node :: Int) == (1 :: Int) then min (5 :: Int) (val + (1 :: Int)) else val
                , edgeTransfer = \_ _ val -> val
                , merge    = max
                , initial  = (0 :: Int)
                }

            result :: Map Int Int
            result = solve (0 :: Int) cfg problem

        Map.lookup (0 :: Int) result `shouldBe` Just (0 :: Int)
        Map.lookup (1 :: Int) result `shouldBe` Just (5 :: Int)
        Map.lookup (2 :: Int) result `shouldBe` Just (5 :: Int)
        Map.lookup (3 :: Int) result `shouldBe` Just (5 :: Int)

data MyEdge = MyTrue | MyFalse deriving (Eq, Ord, Show)
