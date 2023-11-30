package at.doml.validators

import at.doml.model.{ ErrorMessage, MapNode, UnparsedInput }
import cats.data.Validated

/**
  * Validator that checks connected neighbour character of the specified character.
  */
abstract class AbstractMultilineConnectionsUnparsedInputValidator extends UnparsedInputValidator {

  def validate(input: UnparsedInput): Validated[List[ErrorMessage], UnparsedInput] = {
    // pad input to minimum of three lines (or to two lines for empty input)
    val paddedList = "" :: input.toList ::: "" :: Nil

    // check every 3 lines and ignore the case when there are less than 3 lines - because of the previous padding, this
    // will only happen when the input is empty
    val errors = paddedList.sliding(3).zipWithIndex.flatMap {
      case (previous :: current :: next :: Nil, lineIndex) => validateLine(previous, current, next, lineIndex + 1)
      case _                                               => Nil
    }.toList

    errors match {
      case Nil => valid(input)
      case _   => Validated.Invalid(errors)
    }
  }

  protected case class ConnectedNeighbours(left: Char, right: Char, above: Char, below: Char) {
    def count(fn: Char => Boolean): Int = List(left, right, above, below).count(fn)
  }

  extension (s: String) {
    private def getOrEmpty(i: Int, ignore: Char): Char = {
      val char = s.applyOrElse(i, _ => MapNode.EmptySpace.char)
      if char == ignore then MapNode.EmptySpace.char else char
    }
  }

  /**
    * Checks that all characters in the line are valid.
    *
    * @param previous   line above the line which is being checked.
    * @param current    input line which is being checked.
    * @param next       line below the line which is being checked.
    * @param lineNumber number of the line that is being checked.
    * @return List of error messages for this line.
    */
  private def validateLine(previous: String, current: String, next: String, lineNumber: Int): List[ErrorMessage] =
    current.zipWithIndex
      .filter { case (char, _) => charFilter(char) }
      .flatMap { case (char, i) =>
        // get neighbours of the current character (or empty space for non-existent neighbours) - disjoint paths will be
        // ignored and treated as empty spaces
        val left  = current.getOrEmpty(i - 1, ignore = MapNode.VerticalLine.char)
        val right = current.getOrEmpty(i + 1, ignore = MapNode.VerticalLine.char)
        val above = previous.getOrEmpty(i, ignore = MapNode.HorizontalLine.char)
        val below = next.getOrEmpty(i, ignore = MapNode.HorizontalLine.char)

        validateCharacter(ConnectedNeighbours(left, right, above, below), lineNumber, columnNumber = i + 1)
      }.toList

  /**
    * Filter function which specifies which character(s) will be checked by this validator.
    *
    * @param char character to filter.
    * @return `true` if character should be validated, `false` otherwise.
    */
  protected def charFilter(char: Char): Boolean

  /**
    * Validates a single character in the current line.
    *
    * @param connectedNeighbours connected neighbours of the character.
    * @param lineNumber number of the line that is being checked.
    * @param columnNumber number of the column that is being checked.
    * @return [[None]] if the character is valid, error message wrapped in [[Some]] otherwise.
    */
  protected def validateCharacter(
    connectedNeighbours: ConnectedNeighbours,
    lineNumber:          Int,
    columnNumber:        Int
  ): Option[ErrorMessage]
}
