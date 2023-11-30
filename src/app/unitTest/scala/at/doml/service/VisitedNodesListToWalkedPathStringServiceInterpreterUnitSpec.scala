package at.doml.service

import at.doml.model.{ MapNode, MapPosition, VisitedMapNode }
import cats.effect.IO
import test.UnitSpec

class VisitedNodesListToWalkedPathStringServiceInterpreterUnitSpec extends UnitSpec {

  "VisitedNodesListToWalkedPathStringServiceInterpreter" - {

    val service = new VisitedNodesListToWalkedPathStringServiceInterpreter[IO]

    def visitedNodes(nodes: (MapNode, Int, Int)*)(expected: String) =
      service.collectVisitedNodes(nodes.map(n => VisitedMapNode(n._1, MapPosition(n._2, n._3))).toList)
        .asserting(_ shouldBe expected)

    fn"collectVisitedNodes(List[VisitedMapNode])" - {

      "should correctly create a string of visited path nodes" - {

        "when no nodes were visited" in visitedNodes()("")

        "when some path was visited" in visitedNodes(
          (MapNode.Start, 0, 2),
          (MapNode.HorizontalLine, 1, 2),
          (MapNode.Letter('A'), 2, 2),
          (MapNode.HorizontalLine, 3, 2),
          (MapNode.Letter('B'), 4, 2),
          (MapNode.VerticalLine, 4, 3),
          (MapNode.Letter('C'), 4, 4),
          (MapNode.HorizontalLine, 3, 4),
          (MapNode.Letter('D'), 2, 4),
          (MapNode.VerticalLine, 2, 3),
          (MapNode.Letter('A'), 2, 2),
          (MapNode.VerticalLine, 2, 1),
          (MapNode.End, 2, 0)
        )("@-A-B|C-D|A|x")
      }
    }
  }
}
