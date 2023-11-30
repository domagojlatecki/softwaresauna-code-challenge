package at.doml

import at.doml.error.{ InputLoadingError, ParsingError, ValidationError }
import at.doml.model.{ InputSource, MapPosition, ParsedMap, UnparsedInput }
import at.doml.service.{ InputLoadingServiceAlgebra, MapParsingServiceAlgebra, ValidationServiceAlgebra }
import cats.effect.IO
import test.UnitSpec
import scala.collection.immutable.ArraySeq

class InputProcessingModuleUnitSpec extends UnitSpec {

  "InputProcessingModule" - {

    val inputSource = InputSource("")
    val emptyMap    = ParsedMap(ArraySeq.empty, MapPosition(0, 0))

    given InputLoadingServiceAlgebra[IO] = _ => IO(UnparsedInput(ArraySeq.empty))
    given ValidationServiceAlgebra[IO]   = input => IO(input)
    given MapParsingServiceAlgebra[IO]   = _ => IO(emptyMap)

    fn"loadMap(InputSource)" - {

      "should correctly load map" in {
        val module = new InputProcessingModule[IO]

        module.loadMap(inputSource)
          .asserting(_ shouldBe emptyMap)
      }

      "should raise error" - {

        "when input loading fails" in {
          val module = {
            given InputLoadingServiceAlgebra[IO] = _ => IO.raiseError(InputLoadingError("", None))
            new InputProcessingModule[IO]
          }

          module.loadMap(inputSource)
            .assertThrowsError(_ shouldBe a[InputLoadingError])
        }

        "when validation fails" in {
          val module = {
            given ValidationServiceAlgebra[IO] = _ => IO.raiseError(ValidationError(Nil))
            new InputProcessingModule[IO]
          }

          module.loadMap(inputSource)
            .assertThrowsError(_ shouldBe a[ValidationError])
        }

        "when parsing fails" in {
          val module = {
            given MapParsingServiceAlgebra[IO] = _ => IO.raiseError(ParsingError(""))
            new InputProcessingModule[IO]
          }

          module.loadMap(inputSource)
            .assertThrowsError(_ shouldBe a[ParsingError])
        }
      }
    }
  }
}
