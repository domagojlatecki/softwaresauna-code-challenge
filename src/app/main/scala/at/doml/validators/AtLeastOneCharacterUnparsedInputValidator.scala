package at.doml.validators

import at.doml.types.{ ErrorMessage, UnparsedInput }
import cats.data.Validated

/**
  * Validator that checks if input contains specified character at least once.
  *
  * @param char character to check for.
  * @param charName name of the character to show in the validation error message.
  */
class AtLeastOneCharacterUnparsedInputValidator(char: Char, charName: String) extends UnparsedInputValidator {

  def validate(input: UnparsedInput): Validated[ErrorMessage, UnparsedInput] =
    input.flatten.count(_ == char) match {
      case 0 => invalid(s"Missing $charName character ('$char') in input")
      case _ => valid(input)
    }
}
