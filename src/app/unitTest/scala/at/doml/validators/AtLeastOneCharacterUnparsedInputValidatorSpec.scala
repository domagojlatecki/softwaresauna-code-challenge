package at.doml.validators

import at.doml.types.{ ErrorMessage, UnparsedInput }
import cats.data.Validated.{ Invalid, Valid }
import cats.effect.IO
import test.UnitSpec
import scala.collection.immutable.ArraySeq

class AtLeastOneCharacterUnparsedInputValidatorSpec extends UnitSpec {

  "AtLeastOneCharacterUnparsedInputValidator" - {
    val validator = new AtLeastOneCharacterUnparsedInputValidator('A', "example")

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

        "when input contains more than one expected character" - {

          "and no other characters" in validInput("AAA")

          "and some other characters" in validInput("AAA Gaming")

          "with multiple lines" in validInput(
            "First line",
            "Second line: AAA",
            "Third Line"
          )
        }
      }

      "should return Invalid(errorMessage)" - {
        val errorMessage = ErrorMessage("Missing example character ('A') in input")

        def invalidInput(lines: String*) = {
          val input = asInput(lines*)

          IO.pure(validator.validate(input))
            .asserting(_ shouldBe Invalid(errorMessage))
        }

        "when input is missing expected character" - {

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
