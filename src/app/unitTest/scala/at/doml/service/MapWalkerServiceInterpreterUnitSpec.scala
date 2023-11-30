package at.doml.service

import at.doml.error.MapWalkingError
import at.doml.model.{ MapNode, MapPosition, ParsedMap, VisitedMapNode }
import cats.effect.IO
import test.UnitSpec
import scala.collection.immutable.ArraySeq

class MapWalkerServiceInterpreterUnitSpec extends UnitSpec {

  "MapWalkerServiceInterpreter" - {

    val service = new MapWalkerServiceInterpreter[IO]

    def parseTestMap(startX: Int, startY: Int, lines: String*): ParsedMap =
      ParsedMap(lines.map(l => l.map(MapNode.fromChar).to(ArraySeq)).to(ArraySeq), MapPosition(startX, startY))

    fn"walkOnMap(ParsedMap, Int)" - {

      "should return visited map nodes in the correct order" - {

        def validMap(startX: Int, startY: Int, lines: String*)(visitedNodes: (MapNode, Int, Int)*) =
          service.walkOnMap(parseTestMap(startX, startY, lines*), maxSteps = 1000)
            .asserting(_ shouldBe visitedNodes.map(vn => VisitedMapNode(vn._1, MapPosition(vn._2, vn._3))))

        "for the simplest valid map" - {

          "starting left" in validMap(1, 0, "x@")(
            (MapNode.Start, 1, 0),
            (MapNode.End, 0, 0)
          )

          "starting right" in validMap(0, 0, "@x")(
            (MapNode.Start, 0, 0),
            (MapNode.End, 1, 0)
          )

          "starting up" in validMap(
            0,
            1,
            "x",
            "@"
          )(
            (MapNode.Start, 0, 1),
            (MapNode.End, 0, 0)
          )

          "starting down" in validMap(
            0,
            0,
            "@",
            "x"
          )(
            (MapNode.Start, 0, 0),
            (MapNode.End, 0, 1)
          )
        }

        "for the simplest valid map ignoring nodes after end node" in validMap(0, 0, "@x---ABC")(
          (MapNode.Start, 0, 0),
          (MapNode.End, 1, 0)
        )

        "for a simple map with one character" in validMap(0, 0, "@-A-x")(
          (MapNode.Start, 0, 0),
          (MapNode.HorizontalLine, 1, 0),
          (MapNode.Letter('A'), 2, 0),
          (MapNode.HorizontalLine, 3, 0),
          (MapNode.End, 4, 0)
        )

        "by taking correct turn direction" - {

          "to the left" in validMap(
            1, 2,
            "x+|",
            " | ",
            " @"
          )(
            (MapNode.Start, 1, 2),
            (MapNode.VerticalLine, 1, 1),
            (MapNode.Turn, 1, 0),
            (MapNode.End, 0, 0)
          )

          "to the right" in validMap(
            1, 2,
            "|+x",
            " | ",
            " @"
          )(
            (MapNode.Start, 1, 2),
            (MapNode.VerticalLine, 1, 1),
            (MapNode.Turn, 1, 0),
            (MapNode.End, 2, 0)
          )

          "to up" in validMap(
            0, 1,
            "  x",
            "@-+",
            "  -"
          )(
            (MapNode.Start, 0, 1),
            (MapNode.HorizontalLine, 1, 1),
            (MapNode.Turn, 2, 1),
            (MapNode.End, 2, 0)
          )

          "to down" in validMap(
            0, 1,
            "  -",
            "@-+",
            "  x"
          )(
            (MapNode.Start, 0, 1),
            (MapNode.HorizontalLine, 1, 1),
            (MapNode.Turn, 2, 1),
            (MapNode.End, 2, 2)
          )
        }

        "for a multiline map with with turns" in validMap(
          0, 0,
          "@-A-+",
          "    |",
          "x--B+"
        )(
          (MapNode.Start, 0, 0),
          (MapNode.HorizontalLine, 1, 0),
          (MapNode.Letter('A'), 2, 0),
          (MapNode.HorizontalLine, 3, 0),
          (MapNode.Turn, 4, 0),
          (MapNode.VerticalLine, 4, 1),
          (MapNode.Turn, 4, 2),
          (MapNode.Letter('B'), 3, 2),
          (MapNode.HorizontalLine, 2, 2),
          (MapNode.HorizontalLine, 1, 2),
          (MapNode.End, 0, 2)
        )

        "for a multiline map with character turns" in validMap(
          0, 0,
          "@-A-B",
          "    |",
          "x--DC"
        )(
          (MapNode.Start, 0, 0),
          (MapNode.HorizontalLine, 1, 0),
          (MapNode.Letter('A'), 2, 0),
          (MapNode.HorizontalLine, 3, 0),
          (MapNode.Letter('B'), 4, 0),
          (MapNode.VerticalLine, 4, 1),
          (MapNode.Letter('C'), 4, 2),
          (MapNode.Letter('D'), 3, 2),
          (MapNode.HorizontalLine, 2, 2),
          (MapNode.HorizontalLine, 1, 2),
          (MapNode.End, 0, 2)
        )

        "for a multiline map with intersection" in validMap(
          0, 2,
          "  x",
          "  |",
          "@---A",
          "  | |",
          "  C-B"
        )(
          (MapNode.Start, 0, 2),
          (MapNode.HorizontalLine, 1, 2),
          (MapNode.HorizontalLine, 2, 2),
          (MapNode.HorizontalLine, 3, 2),
          (MapNode.Letter('A'), 4, 2),
          (MapNode.VerticalLine, 4, 3),
          (MapNode.Letter('B'), 4, 4),
          (MapNode.HorizontalLine, 3, 4),
          (MapNode.Letter('C'), 2, 4),
          (MapNode.VerticalLine, 2, 3),
          (MapNode.HorizontalLine, 2, 2),
          (MapNode.VerticalLine, 2, 1),
          (MapNode.End, 2, 0)
        )

        "for a multiline map with character crossing" in validMap(
          0, 2,
          "  x",
          "  |",
          "@-A-B",
          "  | |",
          "  D-C"
        )(
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
        )

        "for compact multiline map" in validMap(
          0, 1,
          " x",
          "@A+",
          " ++"
        )(
          (MapNode.Start, 0, 1),
          (MapNode.Letter('A'), 1, 1),
          (MapNode.Turn, 2, 1),
          (MapNode.Turn, 2, 2),
          (MapNode.Turn, 1, 2),
          (MapNode.Letter('A'), 1, 1),
          (MapNode.End, 1, 0)
        )
      }

      "should raise MapWalkingError" - {
        def invalidMap(startX: Int, startY: Int, maxSteps: Int, lines: String*)(errorMessage: String) =
          service.walkOnMap(parseTestMap(startX, startY, lines*), maxSteps = maxSteps)
            .assertThrowsError(_ shouldBe MapWalkingError(errorMessage))

        "when starting direction cannot be determined" in invalidMap(startX = 0, startY = 0, maxSteps = 10, "@")(
          "Cannot determine starting direction"
        )

        "when map has broken path" in invalidMap(startX = 0, startY = 0, maxSteps = 10, "@- x")(
          "Empty space was encountered - map has a broken path"
        )

        "when maximum number of steps was reached" in invalidMap(startX = 0, startY = 0, maxSteps = 1, "@--x")(
          "Could not reach the end of the map in specified number of steps"
        )
      }
    }
  }
}
