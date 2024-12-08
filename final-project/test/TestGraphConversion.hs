module TestGraphConversion where

import Data.Vector as V
import GraphTypes
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Hspec
import TspLibParser
import TspTypes

runGraphConversionTests :: IO ()
runGraphConversionTests =
  hspec $ do
    describe "Basic to Node" $ do
      it "converts empty" $ do
        let emptyGraph = BasicGraph [] []
        let result = basicToNode emptyGraph
        result `shouldBe` NodeGraph []
      it "converts single" $ do
        let singleGraph = BasicGraph [NodeId 1] []
        let result = basicToNode singleGraph
        result `shouldBe` NodeGraph [Node (NodeId 1) []]
      it "converts two" $ do
        let twoGraph =
              BasicGraph
                [NodeId 1, NodeId 2]
                [Edge (NodeId 1, NodeId 2, EdgeWeight 1)]
        let result = basicToNode twoGraph
        result `shouldBe`
          NodeGraph
            [Node (NodeId 1) [(NodeId 2, EdgeWeight 1)], Node (NodeId 2) []]
    describe "Node to Basic" $ do
      it "converts empty" $ do
        let emptyGraph = NodeGraph []
        let result = nodeToBasic emptyGraph
        result `shouldBe` BasicGraph [] []
      it "converts single" $ do
        let singleGraph = NodeGraph [Node (NodeId 1) []]
        let result = nodeToBasic singleGraph
        result `shouldBe` BasicGraph [NodeId 1] []
      it "converts two" $ do
        let twoGraph =
              NodeGraph
                [Node (NodeId 1) [(NodeId 2, EdgeWeight 1)], Node (NodeId 2) []]
        let result = nodeToBasic twoGraph
        result `shouldBe`
          BasicGraph
            [NodeId 1, NodeId 2]
            [Edge (NodeId 1, NodeId 2, EdgeWeight 1)]
    describe "Geo TSP Problems" $ do
      it "converts burma14" $ do
        let burma14Path = "tsp_problems/burma14.tsp.gz"
        exists <- doesFileExist burma14Path
        if exists
          then do
            result <- parseTspFile burma14Path
            case result of
              Left err -> expectationFailure $ show err
              Right p -> do
                case tspProblemToBasic p of
                  Left err -> expectationFailure $ show err
                  Right _ -> return ()
          else expectationFailure "burma14.tsp.gz not found"
      it "converts ali535" $ do
        let ali535Path = "tsp_problems/ali535.tsp.gz"
        exists <- doesFileExist ali535Path
        if exists
          then do
            result <- parseTspFile ali535Path
            case result of
              Left err -> expectationFailure $ show err
              Right p -> do
                case tspProblemToBasic p of
                  Left err -> expectationFailure $ show err
                  Right _ -> return ()
          else expectationFailure "ali535.tsp.gz not found"
