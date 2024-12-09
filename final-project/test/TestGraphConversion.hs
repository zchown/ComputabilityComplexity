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
    describe "Euc2D TSP Problems" $ do
      it "converts berlin52" $ do
        let berlin52Path = "tsp_problems/berlin52.tsp.gz"
        exists <- doesFileExist berlin52Path
        if exists
          then do
            result <- parseTspFile berlin52Path
            case result of
              Left err -> expectationFailure $ show err
              Right p -> do
                case tspProblemToBasic p of
                  Left err -> expectationFailure $ show err
                  Right _ -> return ()
          else expectationFailure "berlin52.tsp.gz not found"
      it "converts bier127" $ do
        let bier127Path = "tsp_problems/bier127.tsp.gz"
        exists <- doesFileExist bier127Path
        if exists
          then do
            result <- parseTspFile bier127Path
            case result of
              Left err -> expectationFailure $ show err
              Right p -> do
                case tspProblemToBasic p of
                  Left err -> expectationFailure $ show err
                  Right _ -> return ()
          else expectationFailure "bier127.tsp.gz not found"
      it "converts brd14051" $ do
        let brd14051Path = "tsp_problems/brd14051.tsp.gz"
        exists <- doesFileExist brd14051Path
        if exists
          then do
            result <- parseTspFile brd14051Path
            case result of
              Left err -> expectationFailure $ show err
              Right p -> do
                case tspProblemToBasic p of
                  Left err -> expectationFailure $ show err
                  Right _ -> return ()
          else expectationFailure "brd14051.tsp.gz not found"
      it "converts ch130" $ do
        let ch130Path = "tsp_problems/ch130.tsp.gz"
        exists <- doesFileExist ch130Path
        if exists
          then do
            result <- parseTspFile ch130Path
            case result of
              Left err -> expectationFailure $ show err
              Right p -> do
                case tspProblemToBasic p of
                  Left err -> expectationFailure $ show err
                  Right _ -> return ()
          else expectationFailure "ch130.tsp.gz not found"
      it "converts ch150" $ do
        let ch150Path = "tsp_problems/ch150.tsp.gz"
        exists <- doesFileExist ch150Path
        if exists
          then do
            result <- parseTspFile ch150Path
            case result of
              Left err -> expectationFailure $ show err
              Right p -> do
                case tspProblemToBasic p of
                  Left err -> expectationFailure $ show err
                  Right _ -> return ()
          else expectationFailure "ch150.tsp.gz not found"
      it "converts d198" $ do
        let d198Path = "tsp_problems/d198.tsp.gz"
        exists <- doesFileExist d198Path
        if exists
          then do
            result <- parseTspFile d198Path
            case result of
              Left err -> expectationFailure $ show err
              Right p -> do
                case tspProblemToBasic p of
                  Left err -> expectationFailure $ show err
                  Right _ -> return ()
          else expectationFailure "d198.tsp.gz not found"
      it "converts d493" $ do
        let d493Path = "tsp_problems/d493.tsp.gz"
        exists <- doesFileExist d493Path
        if exists
          then do
            result <- parseTspFile d493Path
            case result of
              Left err -> expectationFailure $ show err
              Right p -> do
                case tspProblemToBasic p of
                  Left err -> expectationFailure $ show err
                  Right _ -> return ()
          else expectationFailure "d493.tsp.gz not found"
      it "converts d657" $ do
        let d657Path = "tsp_problems/d657.tsp.gz"
        exists <- doesFileExist d657Path
        if exists
          then do
            result <- parseTspFile d657Path
            case result of
              Left err -> expectationFailure $ show err
              Right p -> do
                case tspProblemToBasic p of
                  Left err -> expectationFailure $ show err
                  Right _ -> return ()
          else expectationFailure "d657.tsp.gz not found"
      it "converts eil51" $ do
        let eil51Path = "tsp_problems/eil51.tsp.gz"
        exists <- doesFileExist eil51Path
        if exists
          then do
            result <- parseTspFile eil51Path
            case result of
              Left err -> expectationFailure $ show err
              Right p -> do
                case tspProblemToBasic p of
                  Left err -> expectationFailure $ show err
                  Right _ -> return ()
          else expectationFailure "eil51.tsp.gz not found"
