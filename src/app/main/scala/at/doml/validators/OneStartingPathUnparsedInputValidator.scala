package at.doml.validators

import at.doml.model.{ ErrorMessage, MapNode }

/**
  * Validator which checks if starting node has only one connected path.
  */
class OneStartingPathUnparsedInputValidator
    extends AbstractMultilineConnectionsUnparsedInputValidator(MapNode.Start.char) {

  protected override def validateCharacter(
    connectedNeighbours: ConnectedNeighbours,
    lineNumber:          Int,
    columnNumber:        Int
  ): Option[ErrorMessage] = {
    val emptySpaceCount = connectedNeighbours.count(_ == MapNode.EmptySpace.char)

    emptySpaceCount match {
      case 3 => None
      case 4 => Some(ErrorMessage(s"Starting node has no paths at line: $lineNumber, column: $columnNumber"))
      case _ => Some(ErrorMessage(s"Starting node has more than one path at line: $lineNumber, column: $columnNumber"))
    }
  }
}
