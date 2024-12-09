module TestTspSolver where

import GraphTypes
import Kruskal
import Test.Hspec
import TspSolvers

runTspSolverTests :: IO ()
runTspSolverTests =
  hspec $ do
    describe "TspSolvers" $ do
      it "problem from the book" $ do
        let bg =
              BasicGraph
                [NodeId 1, NodeId 2, NodeId 3, NodeId 4, NodeId 5, NodeId 6]
                [ Edge (NodeId 1, NodeId 2, EdgeWeight 1)
                , Edge (NodeId 1, NodeId 3, EdgeWeight 4)
                , Edge (NodeId 1, NodeId 4, EdgeWeight 7)
                , Edge (NodeId 1, NodeId 5, EdgeWeight 6)
                , Edge (NodeId 1, NodeId 6, EdgeWeight 2)
                , Edge (NodeId 2, NodeId 3, EdgeWeight 5)
                , Edge (NodeId 2, NodeId 4, EdgeWeight 8)
                , Edge (NodeId 2, NodeId 5, EdgeWeight 7)
                , Edge (NodeId 2, NodeId 6, EdgeWeight 3)
                , Edge (NodeId 3, NodeId 4, EdgeWeight 3)
                , Edge (NodeId 3, NodeId 5, EdgeWeight 2)
                , Edge (NodeId 3, NodeId 6, EdgeWeight 6)
                , Edge (NodeId 4, NodeId 5, EdgeWeight 5)
                , Edge (NodeId 4, NodeId 6, EdgeWeight 9)
                , Edge (NodeId 5, NodeId 6, EdgeWeight 8)
                ]
        let mst = kruskal bg
        (tspCost . tspSolver) mst `shouldBe` 24.0
