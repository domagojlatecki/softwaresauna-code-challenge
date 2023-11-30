package at.doml

import at.doml.error.{ InputLoadingError, ValidationError }
import at.doml.model.{ ErrorMessage, InputSource, MapNode, MapPosition, ParsedMap }
import cats.effect.IO
import cats.effect.IO.asyncForIO
import test.IntegSpec
import scala.collection.immutable.ArraySeq

class InputProcessingModuleIntegSpec extends IntegSpec {

  "InputProcessingModule" - {

    val testFilesFolderPath = getClass.getClassLoader.getResource("input-processing-tests").getPath
    val createModule        = InputProcessingModule.create[IO]

    fn"loadMap(InputSource)" - {

      "should correctly load some valid map" in {
        val result = for {
          module    <- createModule
          parsedMap <- module.loadMap(InputSource(s"$testFilesFolderPath/valid/example.txt"))
        } yield parsedMap

        result.asserting(
          _ shouldBe ParsedMap(
            elements      = ArraySeq(
              ArraySeq(MapNode.Start, MapNode.HorizontalLine, MapNode.Letter('A'), MapNode.Turn),
              ArraySeq(MapNode.EmptySpace, MapNode.EmptySpace, MapNode.EmptySpace, MapNode.VerticalLine),
              ArraySeq(MapNode.End, MapNode.Letter('C'), MapNode.Letter('B'), MapNode.Turn)
            ),
            startPosition = MapPosition(0, 0)
          )
        )
      }

      "should raise error for non-existent map file" in {
        val result = for {
          module <- createModule
          _      <- module.loadMap(InputSource(s"$testFilesFolderPath/non-existent-file"))
        } yield ()

        result.assertThrowsError {
          case InputLoadingError(message, _) => message shouldBe "Could not load data from provided file"
          case other                         => fail(s"Expected InputLoadingError, but got: ${other.getClass}")
        }
      }

      "should raise error for non-valid input map" - {
        def expectedValidationError(fileName: String)(errorMessage: String) = {
          val result = for {
            module <- createModule
            _      <- module.loadMap(InputSource(s"$testFilesFolderPath/invalid/$fileName"))
          } yield ()

          result.assertThrowsError(_ shouldBe ValidationError(ErrorMessage(errorMessage) :: Nil))
        }

        "with missing start character" in expectedValidationError("missing-start.txt")(
          "Missing start character ('@') in input"
        )

        "with missing end character" in expectedValidationError("missing-end.txt")(
          "Missing end character ('x') in input"
        )

        "with multiple start characters" in expectedValidationError("multiple-starts.txt")(
          "Only one start character ('@') is expected in input, but 3 were found"
        )

        "with multiple start paths" in expectedValidationError("multiple-starting-paths.txt")(
          "Starting node has more than one path at line: 1, column: 5"
        )

        "with fake turn" in expectedValidationError("fake-turn.txt")(
          "Fake turn at line: 1, column: 5"
        )

        "with fork in path" in expectedValidationError("fork-in-path.txt")(
          "Fork in path at line: 3, column: 8"
        )

        "with fork in letter path" in expectedValidationError("fork-in-letter-path.txt")(
          "Fork in path at line: 3, column: 8"
        )

        "with non-allowed-characters" in expectedValidationError("non-allowed-characters.txt")(
          "Invalid character(s) in input: 'n', 'o', 't', 'a', 'l', 'w', 'e', 'd'"
        )
      }
    }
  }
}
