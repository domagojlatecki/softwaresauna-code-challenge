package at.doml.validators

import at.doml.types.{ ErrorMessage, UnparsedInput }
import cats.data.Validated.{ Invalid, Valid }
import cats.effect.IO
import test.UnitSpec
import scala.collection.immutable.ArraySeq

class ExactlyOneCharacterUnparsedInputValidatorSpec extends UnitSpec {

  "ExactlyOneCharacterUnparsedInputValidator" - {
    val validator = new ExactlyOneCharacterUnparsedInputValidator('A', "example")

    def asInput(lines: String*) =
      UnparsedInput(lines.to(ArraySeq))

    fn"validate(UnparsedInput)" - {

      "should return Valid(input)" - {
        def validInput(lines: String*) = {
          val input = asInput(lines*)

          IO.pure(validator.validate(input))
            .asserting(_ shouldBe Valid(input))
        }

        "when input contains exactly one expected character" - {

          "and no other characters" in validInput("A")

          "and some other characters" in validInput("Sentence with 'A' character")

          "with multiple lines" in validInput(
            "First line without expected character",
            "Second line with 'A' character",
            "Third line"
          )
        }
      }

      "should return Invalid(errorMessage)" - {
        def invalidInput(lines: String*)(using errorMessage: ErrorMessage) = {
          val input = asInput(lines*)

          IO.pure(validator.validate(input))
            .asserting(_ shouldBe Invalid(errorMessage))
        }

        "when input contains more than one expected character" - {
          given ErrorMessage = ErrorMessage("Only one example character ('A') is expected in input, but 3 were found")

          "and no other characters" in invalidInput("AAA")

          "and some other characters" in invalidInput("AAA Gaming")

          "with multiple lines" in invalidInput(
            "First line",
            "Second line: AAA",
            "Third Line"
          )
        }

        "when input is missing expected character" - {
          given ErrorMessage = ErrorMessage("Missing example character ('A') in input")

          "and no other characters" in invalidInput("")

          "and some other characters" in invalidInput("This sentence is missing the required character")

          "with multiple lines" in invalidInput(
            "Lorem ipsum",
            "dolor sit",
            "amet"
          )
        }
      }
    }
  }
}
