package at.doml.service

import at.doml.error.ValidationError
import at.doml.model.{ ErrorMessage, UnparsedInput }
import at.doml.validators.UnparsedInputValidator
import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import cats.effect.Sync
import cats.implicits.*

/**
  * Service used to validate program input.
  *
  * @tparam F effect wrapper type.
  */
trait ValidationServiceAlgebra[F[_]] {

  /**
    * Validates unparsed input and raises an error if input is not valid.
    *
    * @param input input to validate.
    * @return Validated input or error if validation failed.
    */
  def validate(input: UnparsedInput): F[UnparsedInput]
}

/**
  * Implementation of validator service which uses a list of provided validators to validate program input. The errors
  * are accumulated instead of short-circuiting on first error.
  *
  * @param validators list of input validators.
  * @param F [[Sync]] typeclass instance for the used effect wrapper.
  * @tparam F effect wrapper type.
  */
class ErrorAccumulatingValidationServiceInterpreter[F[_]](
  using
  validators: List[UnparsedInputValidator],
  F:          Sync[F]
) extends ValidationServiceAlgebra[F] {

  // needed so that we can use Validated#combine method with UnparsedInput type
  private given Semigroup[UnparsedInput] = Semigroup.first

  private def valid(input: UnparsedInput): Validated[List[ErrorMessage], UnparsedInput] =
    Validated.Valid(input)

  override def validate(input: UnparsedInput): F[UnparsedInput] =
    for {
      validatedInput <- F.delay {
                          // run all validators sequentially and accumulate error messages
                          validators.foldLeft(valid(input)) { case (acc, validator) =>
                            acc.combine(validator.validate(input))
                          }
                        }
      result         <- validatedInput match {
                          case Valid(i)        => F.pure(i)
                          case Invalid(errors) => F.raiseError(ValidationError(errors))
                        }
    } yield result
}
