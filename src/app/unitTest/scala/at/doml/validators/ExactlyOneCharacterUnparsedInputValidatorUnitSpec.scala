package at.doml.validators

import at.doml.model.ErrorMessage

class ExactlyOneCharacterUnparsedInputValidatorUnitSpec extends AbstractValidatorUnitSpec {

  override def validator = new ExactlyOneCharacterUnparsedInputValidator('A', "example")

  "ExactlyOneCharacterUnparsedInputValidator" - {

    fn"validate(UnparsedInput)" - {

      "should return Valid(input)" - {

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

        "when input contains more than one expected character" - {
          given ErrorMessage = ErrorMessage("Only one example character ('A') is expected in input, but 3 were found")

          "and no other characters" in singleErrorInvalidInput("AAA")

          "and some other characters" in singleErrorInvalidInput("AAA Gaming")

          "with multiple lines" in singleErrorInvalidInput(
            "First line",
            "Second line: AAA",
            "Third Line"
          )
        }

        "when input is missing expected character" - {
          given ErrorMessage = ErrorMessage("Missing example character ('A') in input")

          "and no other characters" in singleErrorInvalidInput("")

          "and some other characters" in singleErrorInvalidInput("This sentence is missing the required character")

          "with multiple lines" in singleErrorInvalidInput(
            "Lorem ipsum",
            "dolor sit",
            "amet"
          )
        }
      }
    }
  }
}
