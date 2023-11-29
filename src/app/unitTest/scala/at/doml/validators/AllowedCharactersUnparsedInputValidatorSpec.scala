package at.doml.validators

import at.doml.model.ErrorMessage

class AllowedCharactersUnparsedInputValidatorSpec extends AbstractValidatorUnitSpec {

  override def validator = new AllowedCharactersUnparsedInputValidator("ABCD".toSet)

  "AllowedCharactersUnparsedInputValidator" - {

    fn"validate(UnparsedInput)" - {

      "should return Valid(input)" - {

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

        "when input contains one unallowed character" - {
          given ErrorMessage = ErrorMessage("Invalid character(s) in input: 'F'")

          "and no valid characters" - {
            "in a single line" in singleErrorInvalidInput("F")
            "across multiple lines" in singleErrorInvalidInput("F", "FF")
          }

          "and some valid characters" - {
            "in a single line" in singleErrorInvalidInput("FAB")
            "across multiple lines" in singleErrorInvalidInput("AB", "FAD", "CDF")
          }
        }

        "when input contains multiple unallowed characters" - {
          given ErrorMessage = ErrorMessage("Invalid character(s) in input: 'F', 'G', 'H'")

          "and no valid characters" - {
            "in a single line" in singleErrorInvalidInput("FGH")
            "across multiple lines" in singleErrorInvalidInput("F", "G", "H")
          }

          "and some valid characters" - {
            "in a single line" in singleErrorInvalidInput("FDAGBHC")
            "across multiple lines" in singleErrorInvalidInput("FBB", "DDGAACCH")
          }
        }
      }
    }
  }
}
