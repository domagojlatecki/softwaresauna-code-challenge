package at.doml.service

import at.doml.error.ValidationError
import at.doml.model.{ ErrorMessage, UnparsedInput }
import at.doml.validators.UnparsedInputValidator
import cats.effect.IO
import test.UnitSpec
import scala.collection.immutable.ArraySeq

class ErrorAccumulatingValidationServiceInterpreterSpec extends UnitSpec {

  "ErrorAccumulatingValidationServiceInterpreter" - {

    val testInput = UnparsedInput(ArraySeq("test"))

    def service(validators: List[UnparsedInputValidator]) = {
      given List[UnparsedInputValidator] = validators
      new ErrorAccumulatingValidationServiceInterpreter[IO]
    }

    def alwaysValidValidator = new UnparsedInputValidator {
      override def validate(input: UnparsedInput) =
        valid(input)
    }

    def alwaysInvalidValidator(i: Int) = new UnparsedInputValidator {
      override def validate(input: UnparsedInput) =
        invalid(s"Error $i")
    }

    fn"validate(UnparsedInput)" - {

      "should return provided input when all validators return Valid(input)" - {
        def expectedSuccess(validators: UnparsedInputValidator*) =
          service(validators.toList).validate(testInput)
            .asserting(_ shouldBe testInput)

        "when there are no validators specified" in expectedSuccess()

        "when there is one validator specified" in expectedSuccess(alwaysValidValidator)

        "when there are multiple validators specified" in expectedSuccess(
          alwaysValidValidator,
          alwaysValidValidator,
          alwaysValidValidator
        )
      }

      "should raise ValidationError with accumulated error messages" - {
        def expectedValidationError(validators: UnparsedInputValidator*)(errors: String*) =
          service(validators.toList).validate(testInput)
            .assertThrowsError(_ shouldBe ValidationError(errors.map(ErrorMessage(_)).toList))

        "when one validator returns Invalid(errorMessages)" in expectedValidationError(
          alwaysInvalidValidator(1)
        )("Error 1")

        "when multiple validators return Invalid(errorMessages)" - {

          "with some of the validators return Valid(input)" in expectedValidationError(
            alwaysValidValidator,
            alwaysInvalidValidator(1),
            alwaysInvalidValidator(2),
            alwaysValidValidator,
            alwaysInvalidValidator(3)
          )("Error 1", "Error 2", "Error 3")

          "with none of the validators return Valid(input)" in expectedValidationError(
            alwaysInvalidValidator(1),
            alwaysInvalidValidator(2)
          )("Error 1", "Error 2")
        }
      }
    }
  }
}
