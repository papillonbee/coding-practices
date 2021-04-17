package coding_practices.algorithm.graph

import coding_practices.model.graph.{AdjacencyList, Graph, Neighbor}
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DijkstraAlgorithmTest extends AnyFunSpec
  with Matchers
  with OptionValues {

  describe("DijkstraAlgorithm") {
    val citySource: String = "S"
    val cityA: String = "A"
    val cityB: String = "B"
    val cityC: String = "C"
    val cityD: String = "D"
    val cityE: String = "E"
    val cityF: String = "F"
    val cityG: String = "G"
    val cityH: String = "H"
    val cityI: String = "I"
    val cityJ: String = "J"
    val cityK: String = "K"
    val cityL: String = "L"

    val graph: Graph[String] = Graph(
      adjacencyLists = Array(
        AdjacencyList(citySource, Seq(Neighbor(cityA, 7), Neighbor(cityB, 2), Neighbor(cityC, 3))),
        AdjacencyList(cityA, Seq(Neighbor(citySource, 7), Neighbor(cityB, 3), Neighbor(cityD, 4))),
        AdjacencyList(cityB, Seq(Neighbor(citySource, 2), Neighbor(cityA, 3), Neighbor(cityD, 4), Neighbor(cityH, 1))),
        AdjacencyList(cityC, Seq(Neighbor(citySource, 3), Neighbor(cityL, 2))),
        AdjacencyList(cityD, Seq(Neighbor(cityA, 4), Neighbor(cityB, 4), Neighbor(cityF, 5))),
        AdjacencyList(cityE, Seq(Neighbor(cityG, 2), Neighbor(cityK, 5))),
        AdjacencyList(cityF, Seq(Neighbor(cityD, 5), Neighbor(cityH, 3))),
        AdjacencyList(cityG, Seq(Neighbor(cityE, 2), Neighbor(cityH, 2))),
        AdjacencyList(cityH, Seq(Neighbor(cityB, 1), Neighbor(cityF, 3), Neighbor(cityG, 2))),
        AdjacencyList(cityI, Seq(Neighbor(cityJ, 6), Neighbor(cityK, 4), Neighbor(cityL, 4))),
        AdjacencyList(cityJ, Seq(Neighbor(cityI, 6), Neighbor(cityK, 4), Neighbor(cityL, 4))),
        AdjacencyList(cityK, Seq(Neighbor(cityE, 5), Neighbor(cityI, 4), Neighbor(cityJ, 4))),
        AdjacencyList(cityL, Seq(Neighbor(cityC, 2), Neighbor(cityI, 4), Neighbor(cityJ, 4))),
      )
    )
    val dijkstraAlgorithm: DijkstraAlgorithm[String] = new DijkstraAlgorithmImpl[String](graph, citySource)

    it("should solve for a path from source to the given vertex with shortest distance") {
      dijkstraAlgorithm.getShortestDistanceFromSourceToVertex(cityE).value shouldEqual 7
      dijkstraAlgorithm.getShortestPathFromSourceToVertex(cityE) shouldEqual Seq(citySource, cityB, cityH, cityG, cityE)
    }
  }
}
