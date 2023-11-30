package at.doml.model

/**
  * Zero-indexed position on the input map.
  *
  * @param x map column (counted from left to right).
  * @param y map row (counted from top to bottom).
  */
case class MapPosition(x: Int, y: Int) {

  /**
    * Get position in the specified direction relative to this position.
    *
    * @param direction direction in which to get relative position.
    * @return Map position relative to the provided direction.
    */
  def inDirection(direction: Direction): MapPosition =
    MapPosition(x + direction.dx, y + direction.dy)
}
