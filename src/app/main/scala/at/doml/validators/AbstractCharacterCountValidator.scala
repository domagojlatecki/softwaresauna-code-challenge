package at.doml.validators

import at.doml.model.{ ErrorMessage, UnparsedInput }
import cats.data.Validated

/**
  * Abstract validator that checks count of the target character.
  * @param char character to check for.
  */
abstract class AbstractCharacterCountValidator(char: Char) extends UnparsedInputValidator {

  def validate(input: UnparsedInput): Validated[List[ErrorMessage], UnparsedInput] =
    validateCount(input.flatten.count(_ == char)) match {
      case Some(errorMessage) => invalid(errorMessage)
      case None               => valid(input)
    }

  /**
    * Validates count of the target character.
    * @param count number of times the target character was found.
    * @return [[None]] if count is valid, error message wrapped in [[Some]] otherwise.
    */
  protected def validateCount(count: Int): Option[String]
}
