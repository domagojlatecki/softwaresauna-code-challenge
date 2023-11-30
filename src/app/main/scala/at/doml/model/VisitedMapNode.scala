package at.doml.model

/**
  * Represents visited map node with its position.
  *
  * @param node visited map node.
  * @param position position of the node on the map.
  */
case class VisitedMapNode(node: MapNode, position: MapPosition)
