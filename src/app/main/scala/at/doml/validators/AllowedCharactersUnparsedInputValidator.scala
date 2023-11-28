package at.doml.validators

import at.doml.types.{ ErrorMessage, UnparsedInput }
import cats.data.Validated

/**
  * Validator that checks if input contains only allowed set of characters.
  * @param allowedCharacters set of characters that are allowed in the input.
  */
class AllowedCharactersUnparsedInputValidator(allowedCharacters: Set[Char]) extends UnparsedInputValidator {

  def validate(input: UnparsedInput): Validated[ErrorMessage, UnparsedInput] =
    input.flatten.filterNot(allowedCharacters).distinct.toList match {
      case Nil               => valid(input)
      case invalidCharacters =>
        invalid(s"Invalid character(s) in input: ${invalidCharacters.mkString(start = "'", sep = "', '", end = "'")}")
    }
}
