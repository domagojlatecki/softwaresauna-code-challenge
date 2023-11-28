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
