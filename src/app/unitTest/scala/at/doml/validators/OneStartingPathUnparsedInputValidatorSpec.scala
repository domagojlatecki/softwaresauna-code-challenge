package at.doml.validators

import at.doml.model.{ ErrorMessage, UnparsedInput }
import cats.data.Validated.{ Invalid, Valid }
import cats.effect.IO
import test.UnitSpec
import scala.collection.immutable.ArraySeq

class OneStartingPathUnparsedInputValidatorSpec extends UnitSpec {

  "OneStartingPathUnparsedInputValidator" - {
    val validator = new OneStartingPathUnparsedInputValidator

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

        "when input has no start characters" - {
          "in a single line" in validInput("A---x")
          "across multiple lines" in validInput("A---x", "ABC", "BB")
        }

        "when input has one start character" - {
          "connected to end character" - {

            "left" in validInput("x@")

            "right" in validInput("@x")

            "up" in validInput(
              "x",
              "@"
            )

            "down" in validInput(
              "@",
              "x"
            )

            "with disjoint paths" - {
              "left" in validInput(
                " -",
                "x@|",
                " -"
              )

              "right" in validInput(
                " -",
                "|@x",
                " -"
              )

              "up" in validInput(
                " x",
                "|@|",
                " -"
              )

              "down" in validInput(
                " -",
                "|@|",
                " x"
              )
            }
          }

          "connected to path character" - {

            "left" in validInput("-@")

            "right" in validInput("@-")

            "up" in validInput(
              "|",
              "@"
            )

            "down" in validInput(
              "@",
              "|"
            )

            "with disjoint paths" - {
              "left" in validInput(
                " -",
                "-@|",
                " -"
              )

              "right" in validInput(
                " -",
                "|@-",
                " -"
              )

              "up" in validInput(
                " |",
                "|@|",
                " -"
              )

              "down" in validInput(
                " -",
                "|@|",
                " |"
              )
            }
          }
        }

        "when input has multiple start characters" - {
          "in a single line" in validInput("@--@")
          "across multiple lines" in validInput("@---x", "", "@@")
        }
      }

      "should return Invalid(errorMessages)" - {
        def invalidInput(lines: String*)(errors: ErrorMessage*) = {
          val input = asInput(lines*)

          IO.pure(validator.validate(input))
            .asserting(_ shouldBe Invalid(errors.toList))
        }

        def disconnectedStartError(line: Int, column: Int) =
          ErrorMessage(s"Starting node has no paths at line: $line, column: $column")

        def disconnectedStart(lines: String*)(line: Int, column: Int) =
          invalidInput(lines*)(disconnectedStartError(line, column))

        def tooManyStartPathsError(line: Int, column: Int) =
          ErrorMessage(s"Starting node has more than one path at line: $line, column: $column")

        def tooManyStartPaths(lines: String*)(line: Int, column: Int) =
          invalidInput(lines*)(tooManyStartPathsError(line, column))

        "when input contains only one start character" in disconnectedStart("@")(line = 1, column = 1)

        "when input contains only one start character with disjoint paths" in disconnectedStart(
          " -",
          "|@|",
          " -"
        )(line = 2, column = 2)

        "when single start character has multiple connections" in tooManyStartPaths(
          " |",
          "-@-",
          " |"
        )(line = 2, column = 2)

        "when multiple start characters have no connections" in invalidInput(
          "@ @",
          " @ ",
          "@ @"
        )(
          disconnectedStartError(line = 1, column = 1),
          disconnectedStartError(line = 1, column = 3),
          disconnectedStartError(line = 2, column = 2),
          disconnectedStartError(line = 3, column = 1),
          disconnectedStartError(line = 3, column = 3)
        )

        "when multiple start characters have multiple connections" in invalidInput(
          "@@@",
          "@@@",
          "@@@"
        )(
          tooManyStartPathsError(line = 1, column = 1),
          tooManyStartPathsError(line = 1, column = 2),
          tooManyStartPathsError(line = 1, column = 3),
          tooManyStartPathsError(line = 2, column = 1),
          tooManyStartPathsError(line = 2, column = 2),
          tooManyStartPathsError(line = 2, column = 3),
          tooManyStartPathsError(line = 3, column = 1),
          tooManyStartPathsError(line = 3, column = 2),
          tooManyStartPathsError(line = 3, column = 3)
        )
      }
    }
  }
}
