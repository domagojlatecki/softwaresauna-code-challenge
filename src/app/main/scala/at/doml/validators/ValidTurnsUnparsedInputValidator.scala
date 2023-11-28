package at.doml.validators

import at.doml.model.{ ErrorMessage, MapNode }

/**
  * Validator which checks if all turns in the input are valid.
  */
class ValidTurnsUnparsedInputValidator extends AbstractMultilineConnectionsUnparsedInputValidator(MapNode.Turn.char) {

  protected override def validateCharacter(
    connectedNeighbours: ConnectedNeighbours,
    lineNumber:          Int,
    columnNumber:        Int
  ): Option[ErrorMessage] = {
    val emptySpaceCount = connectedNeighbours.count(_ == MapNode.EmptySpace.char)

    emptySpaceCount match {
      // covers cases for forks in the path:
      //  |   |
      // -+-, +- etc.
      //  |   |
      case n if n <= 1 => Some(ErrorMessage(s"Fork in path at line: $lineNumber, column: $columnNumber"))

      // covers cases for possible fake and valid turns
      case 2 => checkForFakeOrValidTurn(connectedNeighbours, lineNumber, columnNumber)

      // covers cases for fake turns:
      //        |
      // +, -+, +,  etc.
      //
      case n if n >= 3 => Some(ErrorMessage(s"Fake turn at line: $lineNumber, column: $columnNumber"))
    }
  }

  /**
    * Check for fake or valid turns which have '''exactly two''' empty space neighbours.
    *
    * @param connectedNeighbours connected neighbour characters.
    * @param lineNumber number of the line that is being checked.
    * @param columnNumber number of the column that is being checked.
    * @return [[None]] if the turn is valid, error message wrapped in [[Some]] otherwise.
    */
  private def checkForFakeOrValidTurn(
    connectedNeighbours: ConnectedNeighbours,
    lineNumber:          Int,
    columnNumber:        Int
  ): Option[ErrorMessage] =
    // check cases for:
    //         |
    // -+- and +
    //         |
    if (connectedNeighbours.left == MapNode.EmptySpace.char && connectedNeighbours.right == MapNode.EmptySpace.char) ||
      (connectedNeighbours.above == MapNode.EmptySpace.char && connectedNeighbours.below == MapNode.EmptySpace.char)
    then Some(ErrorMessage(s"Fake turn at line: $lineNumber, column: $columnNumber"))
    else None
}
