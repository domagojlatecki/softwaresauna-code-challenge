package at.doml.service

import at.doml.error.ParsingError
import at.doml.model.{ MapNode, MapPosition, ParsedMap, UnparsedInput }
import cats.effect.IO
import test.UnitSpec
import scala.collection.immutable.ArraySeq

class MapParsingServiceInterpreterUnitSpec extends UnitSpec {

  "MapParsingServiceInterpreter" - {

    val service = MapParsingServiceInterpreter[IO]

    fn"parse(UnparsedInput)" - {

      "should return correct ParsedMap" - {
        def expectedParsedMap(input: String*)(expectedMap: ParsedMap) =
          service.parse(UnparsedInput(input.to(ArraySeq)))
            .asserting(_ shouldBe expectedMap)

        "when input map has one line" in expectedParsedMap("@-Ax")(
          ParsedMap(
            elements      = ArraySeq(
              ArraySeq(MapNode.Start, MapNode.HorizontalLine, MapNode.Letter('A'), MapNode.End)
            ),
            startPosition = MapPosition(0, 0)
          )
        )

        "when input map has multiple lines" in expectedParsedMap(
          "A-+",
          "| |",
          "x @"
        )(
          ParsedMap(
            elements      = ArraySeq(
              ArraySeq(MapNode.Letter('A'), MapNode.HorizontalLine, MapNode.Turn),
              ArraySeq(MapNode.VerticalLine, MapNode.EmptySpace, MapNode.VerticalLine),
              ArraySeq(MapNode.End, MapNode.EmptySpace, MapNode.Start)
            ),
            startPosition = MapPosition(2, 2)
          )
        )
      }

      "should raise ParsingError" - {
        def expectedParsingError(input: String*) =
          service.parse(UnparsedInput(input.to(ArraySeq)))
            .assertThrowsError(_ shouldBe ParsingError("Start node not found on the map"))

        "when input is empty" in expectedParsingError("")

        "when input has no starting position character" in expectedParsingError(
          "+-+",
          "| |",
          "A x"
        )
      }
    }
  }
}
