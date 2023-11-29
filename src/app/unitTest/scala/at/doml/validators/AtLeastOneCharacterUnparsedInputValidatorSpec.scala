package at.doml.validators

import at.doml.model.ErrorMessage

class AtLeastOneCharacterUnparsedInputValidatorSpec extends AbstractValidatorUnitSpec {

  override def validator = new AtLeastOneCharacterUnparsedInputValidator('A', "example")

  "AtLeastOneCharacterUnparsedInputValidator" - {

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
        given ErrorMessage = ErrorMessage("Missing example character ('A') in input")

        "when input is missing expected character" - {

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
