package at.doml.model

/**
  * Enumeration which represents possible map nodes along with their character representation.
  *
  * @param char character which represents this map node.
  */
enum MapNode(val char: Char) {
  case Start           extends MapNode('@')
  case End             extends MapNode('x')
  case Turn            extends MapNode('+')
  case HorizontalLine  extends MapNode('-')
  case VerticalLine    extends MapNode('|')
  case EmptySpace      extends MapNode(' ')
  case Letter(l: Char) extends MapNode(l)
}

object MapNode {

  private val NonLetterNodes: Set[MapNode] = Set(
    MapNode.Start,
    MapNode.End,
    MapNode.Turn,
    MapNode.HorizontalLine,
    MapNode.VerticalLine,
    MapNode.EmptySpace
  )

  val NonLetterCharacters: Set[Char] = NonLetterNodes.map(_.char)

  /**
    * Get enum value of [[MapNode]] for the specified character.
    *
    * @param char character for which to get enum value.
    * @return Value of the [[MapNode]] enum for given character.
    */
  def fromChar(char: Char): MapNode =
    NonLetterNodes.find(_.char == char)
      .getOrElse(MapNode.Letter(char))
}
