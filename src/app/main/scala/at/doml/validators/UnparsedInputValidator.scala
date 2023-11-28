package at.doml.validators

import at.doml.model.{ ErrorMessage, UnparsedInput }
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }

/**
  * Trait for validators which check unparsed input.
  */
trait UnparsedInputValidator {

  /**
    * Validates provided unparsed input.
    *
    * @param input unparsed input to validate.
    * @return If input is valid, `input` parameter value wrapped in [[Valid]] object. Otherwise, list of error messages
    *         wrapped in [[Invalid]] object.
    */
  def validate(input: UnparsedInput): Validated[List[ErrorMessage], UnparsedInput]

  protected def valid(input: UnparsedInput) = Valid(input)

  protected def invalid(errorMessage: String) = Invalid(ErrorMessage(errorMessage) :: Nil)
}
