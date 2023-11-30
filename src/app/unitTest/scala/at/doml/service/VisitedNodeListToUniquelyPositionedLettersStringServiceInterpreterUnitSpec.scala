package at.doml.service

import at.doml.model.{ MapNode, MapPosition, VisitedMapNode }
import cats.effect.IO
import test.UnitSpec

class VisitedNodeListToUniquelyPositionedLettersStringServiceInterpreterUnitSpec extends UnitSpec {

  "VisitedNodeListToUniquelyPositionedLettersStringServiceInterpreter" - {

    val service = new VisitedNodeListToUniquelyPositionedLettersStringServiceInterpreter[IO]

    def visitedNodes(nodes: (MapNode, Int, Int)*)(expected: String) =
      service.collectVisitedNodesToString(nodes.map(n => VisitedMapNode(n._1, MapPosition(n._2, n._3))).toList)
        .asserting(_ shouldBe s"Letters: $expected")

    fn"collectVisitedNodesToString(List[VisitedMapNode])" - {

      "should correctly create a string of uniquely positioned letters" - {

        "when no nodes were visited" in visitedNodes()("")

        "when only letter nodes were visited" - {

          "with no duplicates" in visitedNodes(
            (MapNode.Letter('A'), 0, 0),
            (MapNode.Letter('B'), 0, 1),
            (MapNode.Letter('C'), 1, 0),
            (MapNode.Letter('A'), 3, 3)
          )("ABCA")

          "with some duplicates" in visitedNodes(
            (MapNode.Letter('A'), 0, 0),
            (MapNode.Letter('B'), 0, 1),
            (MapNode.Letter('C'), 1, 0),
            (MapNode.Letter('A'), 3, 3),
            (MapNode.Letter('A'), 0, 0),
            (MapNode.Letter('B'), 0, 1),
            (MapNode.Letter('D'), 5, 3)
          )("ABCAD")
        }

        "when letters and other nodes were visited" - {

          "with no duplicates" in visitedNodes(
            (MapNode.Start, 0, 0),
            (MapNode.Letter('A'), 0, 1),
            (MapNode.Turn, 2, 2),
            (MapNode.Letter('B'), 0, 2),
            (MapNode.Letter('C'), 1, 0),
            (MapNode.Letter('A'), 3, 3),
            (MapNode.End, 1, 1)
          )("ABCA")

          "with some duplicates" in visitedNodes(
            (MapNode.Start, 0, 0),
            (MapNode.Letter('A'), 0, 0),
            (MapNode.Letter('B'), 0, 1),
            (MapNode.Turn, 2, 2),
            (MapNode.Letter('C'), 1, 0),
            (MapNode.Letter('A'), 3, 3),
            (MapNode.Letter('A'), 0, 0),
            (MapNode.Letter('B'), 0, 1),
            (MapNode.Letter('D'), 5, 3),
            (MapNode.End, 1, 1)
          )("ABCAD")
        }
      }
    }
  }
}
