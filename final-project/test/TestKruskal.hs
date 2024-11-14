module TestKruskal where

import GraphTypes
import Kruskal
import Test.Hspec

runTestKruskal :: IO ()
runTestKruskal =
  hspec $ do
    describe "Sort Edges" $ do
      it "sorts empty" $ do
        let result = sortEdges []
        result `shouldBe` ([] :: [Edge])
      it "sorts single" $ do
        let edges = [Edge (NodeId 1, NodeId 2, 3)]
        let result = sortEdges edges
        result `shouldBe` [Edge (NodeId 1, NodeId 2, 3)]
      it "sorts two" $ do
        let edges = [Edge (NodeId 1, NodeId 2, 3), Edge (NodeId 2, NodeId 3, 1)]
        let result = sortEdges edges
        result `shouldBe`
          [Edge (NodeId 2, NodeId 3, 1), Edge (NodeId 1, NodeId 2, 3)]
      it "already sorted" $ do
        let edges = [Edge (NodeId 1, NodeId 2, 1), Edge (NodeId 2, NodeId 3, 3)]
        let result = sortEdges edges
        result `shouldBe`
          [Edge (NodeId 1, NodeId 2, 1), Edge (NodeId 2, NodeId 3, 3)]
      it "split runs" $ do
        let edges =
              [ Edge (NodeId 1, NodeId 1, 1)
              , Edge (NodeId 2, NodeId 2, 2)
              , Edge (NodeId 0, NodeId 0, 0)
              , Edge (NodeId 3, NodeId 3, 3)
              , Edge (NodeId 4, NodeId 4, 4)
              ]
        let result = sortEdges edges
        let correctResult =
              [ Edge (NodeId 0, NodeId 0, 0)
              , Edge (NodeId 1, NodeId 1, 1)
              , Edge (NodeId 2, NodeId 2, 2)
              , Edge (NodeId 3, NodeId 3, 3)
              , Edge (NodeId 4, NodeId 4, 4)
              ]
        result `shouldBe` correctResult
      it "sorts mixed weights" $ do
        let edges =
              [ Edge (NodeId 1, NodeId 2, 5)
              , Edge (NodeId 2, NodeId 3, 2)
              , Edge (NodeId 3, NodeId 4, 4)
              , Edge (NodeId 4, NodeId 1, 1)
              , Edge (NodeId 2, NodeId 4, 3)
              ]
        let result = sortEdges edges
        let correctResult =
              [ Edge (NodeId 4, NodeId 1, 1)
              , Edge (NodeId 2, NodeId 3, 2)
              , Edge (NodeId 2, NodeId 4, 3)
              , Edge (NodeId 3, NodeId 4, 4)
              , Edge (NodeId 1, NodeId 2, 5)
              ]
        result `shouldBe` correctResult
    describe "Kruskal" $ do
      it "handles empty graph" $ do
        let graph = BasicGraph [] []
        let result = kruskal graph
        result `shouldBe` BasicGraph [] []
      it "handles single edge" $ do
        let nodes = [NodeId 1, NodeId 2]
        let edges = [Edge (NodeId 1, NodeId 2, 5)]
        let result = kruskal (BasicGraph nodes edges)
        result `shouldBe` BasicGraph nodes [Edge (NodeId 1, NodeId 2, 5)]
      it "handles simple cycle" $ do
        let nodes = [NodeId 1, NodeId 2, NodeId 3]
        let edges =
              [ Edge (NodeId 1, NodeId 2, 1)
              , Edge (NodeId 2, NodeId 3, 2)
              , Edge (NodeId 3, NodeId 1, 3)
              ]
        let result = kruskal (BasicGraph nodes edges)
        let correctResult =
              BasicGraph
                nodes
                [Edge (NodeId 2, NodeId 3, 2), Edge (NodeId 1, NodeId 2, 1)]
        result `shouldBe` correctResult
      it "handles simple tree" $ do
        let nodes = [NodeId 1, NodeId 2, NodeId 3]
        let edges = [Edge (NodeId 2, NodeId 3, 2), Edge (NodeId 1, NodeId 2, 1)]
        let result = kruskal (BasicGraph nodes edges)
        let correctResult = BasicGraph nodes edges
        result `shouldBe` correctResult
      it "handles complex graph with multiple paths" $ do
        let nodes = [NodeId 1, NodeId 2, NodeId 3, NodeId 4, NodeId 5]
        let edges =
              [ Edge (NodeId 1, NodeId 2, 3)
              , Edge (NodeId 2, NodeId 3, 5)
              , Edge (NodeId 3, NodeId 4, 2)
              , Edge (NodeId 4, NodeId 5, 4)
              , Edge (NodeId 1, NodeId 5, 1)
              , Edge (NodeId 2, NodeId 4, 6)
              , Edge (NodeId 1, NodeId 3, 7)
              ]
        let result = kruskal (BasicGraph nodes edges)
        let correctResult =
              BasicGraph
                nodes
                [ Edge (NodeId 4, NodeId 5, 4)
                , Edge (NodeId 1, NodeId 2, 3)
                , Edge (NodeId 3, NodeId 4, 2)
                , Edge (NodeId 1, NodeId 5, 1)
                ]
        result `shouldBe` correctResult
      it "handles graph with equal weights" $ do
        let nodes = [NodeId 1, NodeId 2, NodeId 3, NodeId 4]
        let edges =
              [ Edge (NodeId 1, NodeId 2, 2)
              , Edge (NodeId 2, NodeId 3, 2)
              , Edge (NodeId 3, NodeId 4, 2)
              , Edge (NodeId 4, NodeId 1, 2)
              ]
        let result = kruskal (BasicGraph nodes edges)
        let correctResult =
              BasicGraph
                nodes
                [ Edge (NodeId 2, NodeId 3, 2)
                , Edge (NodeId 3, NodeId 4, 2)
                , Edge (NodeId 4, NodeId 1, 2)
                ]
        result `shouldBe` correctResult
