package at.doml.validators

import at.doml.types.{ ErrorMessage, UnparsedInput }
import cats.data.Validated

/**
  * Validator that checks if input contains specified character exactly once.
  *
  * @param char character to check for.
  * @param charName name of the character to show in the validation error message.
  */
class ExactlyOneCharacterUnparsedInputValidator(char: Char, charName: String) extends UnparsedInputValidator {

  def validate(input: UnparsedInput): Validated[ErrorMessage, UnparsedInput] =
    input.flatten.count(_ == char) match {
      case 1 => valid(input)
      case 0 => invalid(s"Missing $charName character ('$char') in input")
      case n => invalid(s"Only one $charName character ('$char') is expected in input, but $n were found")
    }
}
