package at.doml.validators

import at.doml.model.{ ErrorMessage, UnparsedInput }
import cats.data.Validated.{ Invalid, Valid }
import cats.effect.IO
import test.UnitSpec
import scala.collection.immutable.ArraySeq

abstract class AbstractValidatorUnitSpec extends UnitSpec {

  def validator: UnparsedInputValidator

  def asInput(lines: String*) =
    UnparsedInput(lines.to(ArraySeq))

  def validInput(lines: String*) = {
    val input = asInput(lines*)

    IO.pure(validator.validate(input))
      .asserting(_ shouldBe Valid(input))
  }

  def multiErrorInvalidInput(lines: String*)(errors: ErrorMessage*) = {
    val input = asInput(lines*)

    IO.pure(validator.validate(input))
      .asserting(_ shouldBe Invalid(errors.toList))
  }

  def singleErrorInvalidInput(lines: String*)(using errorMessage: ErrorMessage) =
    multiErrorInvalidInput(lines*)(errorMessage)

  def forkInPathError(line: Int, column: Int) =
    ErrorMessage(s"Fork in path at line: $line, column: $column")

  def forkInPath(lines: String*)(line: Int, column: Int) =
    multiErrorInvalidInput(lines*)(forkInPathError(line, column))

  def fakeTurnError(line: Int, column: Int) =
    ErrorMessage(s"Fake turn at line: $line, column: $column")

  def fakeTurn(lines: String*)(line: Int, column: Int) =
    multiErrorInvalidInput(lines*)(fakeTurnError(line, column))

  def disconnectedStartError(line: Int, column: Int) =
    ErrorMessage(s"Starting node has no paths at line: $line, column: $column")

  def disconnectedStart(lines: String*)(line: Int, column: Int) =
    multiErrorInvalidInput(lines*)(disconnectedStartError(line, column))

  def tooManyStartPathsError(line: Int, column: Int) =
    ErrorMessage(s"Starting node has more than one path at line: $line, column: $column")

  def tooManyStartPaths(lines: String*)(line: Int, column: Int) =
    multiErrorInvalidInput(lines*)(tooManyStartPathsError(line, column))
}
