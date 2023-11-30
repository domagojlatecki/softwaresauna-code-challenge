package at.doml.model

/**
  * Represents possible movement directions on the map.
  */
enum Direction(val dx: Int, val dy: Int) {

  case Up    extends Direction(-1, 0)
  case Down  extends Direction(1, 0)
  case Left  extends Direction(0, -1)
  case Right extends Direction(0, 1)

  /**
    * @return
    *   Opposite direction of this direction.
    */
  def opposite: Direction = this match {
    case Up    => Down
    case Down  => Up
    case Left  => Right
    case Right => Left
  }

  /**
    * Gets the direction which is considered to be 'left' of this direction. For example, if the current direction is
    * right then the direction to its left is up:<br><br>
    * {{{
    *    ^ relative left direction: up
    *    |
    * ---> current direction: right
    *    |
    *    v relative right direction: down
    * }}}
    * @return
    *   Direction which is left of this direction.
    */
  def relativeLeft: Direction =
    this match {
      case Up    => Left
      case Down  => Right
      case Left  => Down
      case Right => Up
    }

  /**
    * Gets the direction which is considered to be 'right' of this direction. For an example see [[relativeLeft]].
    *
    * @return
    *   Direction which is right of this direction.
    */
  def relativeRight: Direction =
    relativeLeft.opposite
}
