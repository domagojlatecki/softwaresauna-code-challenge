package at.doml.validators

import at.doml.model.{ ErrorMessage, UnparsedInput }
import cats.data.Validated.{ Invalid, Valid }
import cats.effect.IO
import test.UnitSpec
import scala.collection.immutable.ArraySeq

class AllowedCharactersUnparsedInputValidatorSpec extends UnitSpec {

  "AllowedCharactersUnparsedInputValidator" - {
    val validator = new AllowedCharactersUnparsedInputValidator("ABCD".toSet)

    def asInput(lines: String*) =
      UnparsedInput(lines.to(ArraySeq))

    fn"validate(UnparsedInput)" - {

      "should return Valid(input)" - {
        def validInput(lines: String*) = {
          val input = asInput(lines*)

          IO.pure(validator.validate(input))
            .asserting(_ shouldBe Valid(input))
        }

        "when input is empty" in validInput("")

        "when input contains only one of allowed characters" - {
          "only once" in validInput("A")
          "multiple times" in validInput("AAA")
          "across multiple lines" in validInput("AA", "A", "AAA")
        }

        "when input contains only allowed characters" - {
          "in a single line" in validInput("ABCDDDAABCDDA")
          "across multiple lines" in validInput("A", "B", "C", "D", "AA", "BBC")
        }
      }

      "should return Invalid(errorMessage)" - {
        def invalidInput(lines: String*)(using errorMessage: ErrorMessage) = {
          val input = asInput(lines*)

          IO.pure(validator.validate(input))
            .asserting(_ shouldBe Invalid(errorMessage :: Nil))
        }

        "when input contains one unallowed character" - {
          given ErrorMessage = ErrorMessage("Invalid character(s) in input: 'F'")

          "and no valid characters" - {
            "in a single line" in invalidInput("F")
            "across multiple lines" in invalidInput("F", "FF")
          }

          "and some valid characters" - {
            "in a single line" in invalidInput("FAB")
            "across multiple lines" in invalidInput("AB", "FAD", "CDF")
          }
        }

        "when input contains multiple unallowed characters" - {
          given ErrorMessage = ErrorMessage("Invalid character(s) in input: 'F', 'G', 'H'")

          "and no valid characters" - {
            "in a single line" in invalidInput("FGH")
            "across multiple lines" in invalidInput("F", "G", "H")
          }

          "and some valid characters" - {
            "in a single line" in invalidInput("FDAGBHC")
            "across multiple lines" in invalidInput("FBB", "DDGAACCH")
          }
        }
      }
    }
  }
}
