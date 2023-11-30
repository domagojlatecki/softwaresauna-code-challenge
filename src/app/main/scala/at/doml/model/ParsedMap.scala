package at.doml.model

import scala.collection.immutable.ArraySeq

/**
  * Represents parsed input map.
  *
  * @param elements elements of the map. Outer array represents rows while inner array represents columns of a row.
  * @param startPosition position of the start element on the map.
  */
case class ParsedMap(elements: ArraySeq[ArraySeq[MapNode]], startPosition: MapPosition) {

  /**
    * Get a node of the map at the specified position.
    *
    * @param position position for which to fetch map node.
    * @return [[MapNode]] at the specified position, or [[MapNode.EmptySpace]] if there is no map node at the specified
    *         position.
    */
  def apply(position: MapPosition): MapNode =
    elements.applyOrElse(position.y, _ => ArraySeq.empty[MapNode])
      .applyOrElse(position.x, _ => MapNode.EmptySpace)
}
