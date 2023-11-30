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

    given InputLoadingServiceAlgebra[IO] = new InputLoadingServiceAlgebra[IO] {
      override def load(source: InputSource): IO[UnparsedInput] =
        IO(UnparsedInput(ArraySeq.empty))
    }

    given ValidationServiceAlgebra[IO] = new ValidationServiceAlgebra[IO] {
      override def validate(input: UnparsedInput): IO[UnparsedInput] =
        IO(input)
    }

    given MapParsingServiceAlgebra[IO] = new MapParsingServiceAlgebra[IO] {
      override def parse(input: UnparsedInput): IO[ParsedMap] =
        IO(emptyMap)
    }

    fn"loadMap(InputSource)" - {

      "should correctly load map" in {
        val module = new InputProcessingModule[IO]

        module.loadMap(inputSource)
          .asserting(_ shouldBe emptyMap)
      }

      "should raise error" - {

        "when input loading fails" in {
          val module = {
            given InputLoadingServiceAlgebra[IO] = new InputLoadingServiceAlgebra[IO] {
              override def load(source: InputSource): IO[UnparsedInput] =
                IO.raiseError(InputLoadingError("", None))
            }

            new InputProcessingModule[IO]
          }

          module.loadMap(inputSource)
            .assertThrowsError(_ shouldBe a[InputLoadingError])
        }

        "when validation fails" in {
          val module = {
            given ValidationServiceAlgebra[IO] = new ValidationServiceAlgebra[IO] {
              override def validate(input: UnparsedInput): IO[UnparsedInput] =
                IO.raiseError(ValidationError(Nil))
            }

            new InputProcessingModule[IO]
          }

          module.loadMap(inputSource)
            .assertThrowsError(_ shouldBe a[ValidationError])
        }

        "when parsing fails" in {
          val module = {
            given MapParsingServiceAlgebra[IO] = new MapParsingServiceAlgebra[IO] {
              override def parse(input: UnparsedInput): IO[ParsedMap] =
                IO.raiseError(ParsingError(""))
            }

            new InputProcessingModule[IO]
          }

          module.loadMap(inputSource)
            .assertThrowsError(_ shouldBe a[ParsingError])
        }
      }
    }
  }
}
