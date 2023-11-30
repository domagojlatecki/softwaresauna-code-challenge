package at.doml

import at.doml.error.MapWalkingError
import at.doml.model.{ MapNode, MapPosition, ParsedMap }
import cats.effect.IO
import cats.effect.IO.asyncForIO
import test.IntegSpec
import scala.collection.immutable.ArraySeq

class MapProcessingModuleIntegSpec extends IntegSpec {

  "MapProcessingModule" - {

    val createModule = MapProcessingModule.create[IO]

    def parseTestMap(startX: Int, startY: Int, lines: String*): ParsedMap =
      ParsedMap(lines.map(l => l.map(MapNode.fromChar).to(ArraySeq)).to(ArraySeq), MapPosition(startX, startY))

    fn"processMap(ParsedMap, Int)" - {

      "should return correct program output" - {

        def validMap(startX: Int, startY: Int, lines: String*)(letters: String, path: String) = {
          val result = for {
            module        <- createModule
            programResult <- module.processMap(parseTestMap(startX, startY, lines*), maxSteps = 1000)
          } yield programResult

          result.asserting(_ shouldBe s"Letters: $letters\nPath as characters: $path")
        }

        "for a single-line map" in validMap(0, 0, "@-A-B-C-x")(
          letters = "ABC",
          path    = "@-A-B-C-x"
        )

        "for multi-line map" in validMap(
          0, 0,
          "@-A   x",
          "  |   |",
          "  +-B-C-+",
          "      | |",
          "      +-A"
        )(
          letters = "ABCA",
          path    = "@-A|+-B-C-+|A-+|C|x"
        )
      }

      "should raise error" - {

        def invalidMap(startX: Int, startY: Int, maxSteps: Int, lines: String*)(errorMessage: String) = {
          val result = for {
            module        <- createModule
            programResult <- module.processMap(parseTestMap(startX, startY, lines*), maxSteps = maxSteps)
          } yield programResult

          result.assertThrowsError(_ shouldBe MapWalkingError(errorMessage))
        }

        "when map start direction cannot be determined" in invalidMap(startX = 0, startY = 0, maxSteps = 10, "@")(
          "Cannot determine starting direction"
        )

        "when end of the map cannot be reached" in invalidMap(startX = 0, startY = 0, maxSteps = 10, "@- x")(
          "Empty space was encountered - map has a broken path"
        )

        "when maximum number of iterations is reached" in invalidMap(startX = 0, startY = 0, maxSteps = 1, "@--x")(
          "Could not reach the end of the map in specified number of steps"
        )
      }
    }
  }
}
