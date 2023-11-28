package at.doml.validators

import at.doml.model.{ErrorMessage, MapNode, UnparsedInput}
import cats.data.Validated

/**
  * Validator which checks if all turns in the input are valid.
  */
class ValidTurnsUnparsedInputValidator extends UnparsedInputValidator {

  def validate(input: UnparsedInput): Validated[List[ErrorMessage], UnparsedInput] = {
    // pad input to minimum of three lines (or to two lines for empty input)
    val paddedList = "" :: input.toList ::: "" :: Nil

    // check every 3 lines and ignore the case when there are less than 3 lines - because of the previous padding, this
    // will only happen when the input is empty
    val errors = paddedList.sliding(3).zipWithIndex.flatMap {
      case (previous :: current :: next :: Nil, lineIndex) => checkTurnValidity(previous, current, next, lineIndex + 1)
      case _                                               => Nil
    }.toList

    errors match {
      case Nil => valid(input)
      case _   => Validated.Invalid(errors)
    }
  }

  extension (s: String) {
    private def getOrEmpty(i: Int, ignore: Char): Char = {
      val char = s.applyOrElse(i, _ => MapNode.EmptySpace.char)
      if char == ignore then MapNode.EmptySpace.char else char
    }
  }

  /**
    * Checks if all turns in the input line are valid.
    * @param previous line above the line which is being checked.
    * @param current input line for which turns are checked.
    * @param next line below the line which is being checked.
    * @param lineNumber number of the line that is being checked.
    * @return List of error messages for invalid turns in this line.
    */
  private def checkTurnValidity(previous: String, current: String, next: String, lineNumber: Int): List[ErrorMessage] =
    current.zipWithIndex
      .filter { case (char, _) => char == MapNode.Turn.char } // check only turn characters
      .flatMap { case (char, i) =>
        // get neighbours of the current turn character (or empty space for non-existent neighbours) - disjoint paths
        // will be ignored and treated as empty spaces
        val left  = current.getOrEmpty(i - 1, ignore = MapNode.VerticalLine.char)
        val right = current.getOrEmpty(i + 1, ignore = MapNode.VerticalLine.char)
        val above = previous.getOrEmpty(i, ignore = MapNode.HorizontalLine.char)
        val below = next.getOrEmpty(i, ignore = MapNode.HorizontalLine.char)

        val emptySpaceCount = List(left, right, above, below).count(_ == MapNode.EmptySpace.char)
        val columnNumber    = i + 1

        emptySpaceCount match {
          // covers cases for forks in the path:
          //  |   |
          // -+-, +- etc.
          //  |   |
          case n if n <= 1 => Some(ErrorMessage(s"Fork in path at line: $lineNumber, column: $columnNumber"))

          // covers cases for possible fake and valid turns
          case 2 => checkForFakeOrValidTurn(left, right, above, below, lineNumber, columnNumber)

          // covers cases for fake turns:
          //        |
          // +, -+, +,  etc.
          //
          case n if n >= 3 => Some(ErrorMessage(s"Fake turn at line: $lineNumber, column: $columnNumber"))
        }
      }.toList

  /**
    * Check for fake or valid turns which have '''exactly two''' empty space neighbours.
    * @param left left neighbour.
    * @param right right neighbour.
    * @param above above neighbour.
    * @param below below neighbour.
    * @param lineNumber number of the line that is being checked.
    * @param columnNumber number of the column that is being checked.
    * @return [[None]] if the turn is valid, error message wrapped in [[Some]] otherwise.
    */
  private def checkForFakeOrValidTurn(
    left:         Char,
    right:        Char,
    above:        Char,
    below:        Char,
    lineNumber:   Int,
    columnNumber: Int
  ): Option[ErrorMessage] =
    // check cases for:
    //         |
    // -+- and +
    //         |
    if (left == MapNode.EmptySpace.char && right == MapNode.EmptySpace.char) ||
      (above == MapNode.EmptySpace.char && below == MapNode.EmptySpace.char)
    then Some(ErrorMessage(s"Fake turn at line: $lineNumber, column: $columnNumber"))
    else None
}
