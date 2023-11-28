package at.doml.validators

import at.doml.types.{ ErrorMessage, UnparsedInput }
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
    * @return If input is valid, `input` parameter value wrapped in [[Valid]] object. Otherwise, error message wrapped
    *         in [[Invalid]] object.
    */
  def validate(input: UnparsedInput): Validated[ErrorMessage, UnparsedInput]

  protected def valid(input: UnparsedInput) = Valid(input)

  protected def invalid(errorMessage: String) = Invalid(ErrorMessage(errorMessage))
}
