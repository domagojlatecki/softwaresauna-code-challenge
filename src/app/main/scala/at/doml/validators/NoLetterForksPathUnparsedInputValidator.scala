package at.doml.validators

import at.doml.model.{ ErrorMessage, MapNode }

/**
  * Validator which checks if there are no partial forks in turns which contain a letter. Full crossings are allowed for
  *  letter characters.
  */
class NoLetterForksPathUnparsedInputValidator extends AbstractMultilineConnectionsUnparsedInputValidator {

  protected override def charFilter(char: Char): Boolean = !MapNode.NonLetterCharacters.contains(char)

  protected override def validateCharacter(
    connectedNeighbours: ConnectedNeighbours,
    lineNumber:          Int,
    columnNumber:        Int
  ): Option[ErrorMessage] = {
    val emptySpaceCount = connectedNeighbours.count(_ == MapNode.EmptySpace.char)

    if emptySpaceCount == 1 then Some(ErrorMessage(s"Fork in path at line: $lineNumber, column: $columnNumber"))
    else None
  }
}
