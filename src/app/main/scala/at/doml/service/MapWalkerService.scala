package at.doml.service

import at.doml.error.MapWalkingError
import at.doml.model.{ Direction, MapNode, MapPosition, ParsedMap, VisitedMapNode }
import cats.effect.Sync
import cats.implicits.*
import scala.annotation.tailrec

/**
  * Service used to walk on the map path.
  *
  * @tparam F effect wrapper type.
  */
trait MapWalkerServiceAlgebra[F[_]] {

  /**
    * Walks on the provided map with specified maximum amount of steps. Raises an error if the end of the map cannot be
    * reached in the specified amount of steps.
    *
    * @param map map to walk on.
    * @param maxSteps maximum amount of steps before failing.
    * @return List of visited map nodes and their locations in the walk order.
    */
  def walkOnMap(map: ParsedMap, maxSteps: Int): F[List[VisitedMapNode]]
}

/**
  * Implementation of the service used to walk on the map path.
  *
  * @param F [[Sync]] typeclass instance for the used effect wrapper.
  * @tparam F effect wrapper type.
  */
class MapWalkerServiceInterpreter[F[_]](using F: Sync[F]) extends MapWalkerServiceAlgebra[F] {

  override def walkOnMap(map: ParsedMap, maxSteps: Int): F[List[VisitedMapNode]] =
    for {
      startDirection <- findStartingDirection(map)
      walkedNodes    <- advanceStep(
                          map              = map,
                          currentPosition  = map.startPosition,
                          currentDirection = startDirection,
                          remainingSteps   = maxSteps,
                          visitedNodes     = Nil
                        )
    } yield walkedNodes

  /**
    * Finds the starting direction for walking on the map.
    *
    * @param map map that is being walked on.
    * @return Starting direction.
    */
  private def findStartingDirection(map: ParsedMap): F[Direction] = {
    val start = map.startPosition
    val up    = map(start.inDirection(Direction.Up))
    val down  = map(start.inDirection(Direction.Down))
    val left  = map(start.inDirection(Direction.Left))
    val right = map(start.inDirection(Direction.Right))

    if up != MapNode.EmptySpace && up != MapNode.VerticalLine then F.pure(Direction.Up)
    else if down != MapNode.EmptySpace && down != MapNode.VerticalLine then F.pure(Direction.Down)
    else if left != MapNode.EmptySpace && left != MapNode.HorizontalLine then F.pure(Direction.Left)
    else if right != MapNode.EmptySpace && right != MapNode.HorizontalLine then F.pure(Direction.Right)
    else F.raiseError(MapWalkingError("Cannot determine starting direction"))
  }

  /**
    * Recursively walks on the provided input map.
    *
    * @param map map that will be walked on.
    * @param currentPosition current position on the map (start for initial conditions).
    * @param currentDirection current direction on the map (direction of the outward start path for initial conditions).
    * @param remainingSteps recursion step limit.
    * @param visitedNodes list of already visited nodes, in reverse order.
    * @return List of visited nodes.
    */
  @tailrec
  private def advanceStep(
    map:              ParsedMap,
    currentPosition:  MapPosition,
    currentDirection: Direction,
    remainingSteps:   Int,
    visitedNodes:     List[VisitedMapNode]
  ): F[List[VisitedMapNode]] = {
    val currentNode       = map(currentPosition)
    val newVisitedNodes   = VisitedMapNode(currentNode, currentPosition) :: visitedNodes
    val newRemainingSteps = remainingSteps - 1

    currentNode match {

      // path walker reached the end, return the walked path
      case MapNode.End              =>
        F.pure(newVisitedNodes.reverse)

      // empty space was encountered, there was an error in the map definition
      case MapNode.EmptySpace       =>
        F.raiseError(MapWalkingError("Empty space was encountered - map has a broken path"))

      // allowed number of steps was exceeded
      case _ if remainingSteps <= 0 =>
        F.raiseError(MapWalkingError("Could not reach the end of the map in specified number of steps"))

      // handle regular map turns ('+')
      case MapNode.Turn             =>
        val newDirection = calculateNewTurnDirection(map, currentPosition, currentDirection)

        advanceStep(
          map              = map,
          currentPosition  = currentPosition.inDirection(newDirection),
          currentDirection = newDirection,
          remainingSteps   = newRemainingSteps,
          visitedNodes     = newVisitedNodes
        )

      // handle possible turns in letter nodes - when letter node has no valid path ahead
      case MapNode.Letter(_)        =>
        val newDirection = calculateNewLetterDirection(map, currentPosition, currentDirection)

        advanceStep(
          map              = map,
          currentPosition  = currentPosition.inDirection(newDirection),
          currentDirection = newDirection,
          remainingSteps   = newRemainingSteps,
          visitedNodes     = newVisitedNodes
        )

      // handle walking forward on the map
      case _                        =>
        advanceStep(
          map              = map,
          currentPosition  = currentPosition.inDirection(currentDirection),
          currentDirection = currentDirection,
          remainingSteps   = newRemainingSteps,
          visitedNodes     = newVisitedNodes
        )
    }
  }

  /**
    * Calcuales new direction after taking a turn on the map. The new direction can be relatively left or relatively
    * right of the current direction. If no turn direction is valid, current direction is returned.
    *
    * @param map map that is being walked on.
    * @param currentPosition current position on the map.
    * @param currentDirection current movement direction on the map.
    * @return New walking direction on the map.
    */
  private def calculateNewTurnDirection(
    map:              ParsedMap,
    currentPosition:  MapPosition,
    currentDirection: Direction
  ): Direction = {
    val relativeLeft  = checkForConnectedNodeInDirection(map, currentPosition, currentDirection.relativeLeft)
    val relativeRight = checkForConnectedNodeInDirection(map, currentPosition, currentDirection.relativeRight)

    relativeLeft.orElse(relativeRight).getOrElse(currentDirection)
  }

  /**
    * Calculates new direction for the current letter node. If the node ahead of the current letter node is considered
    * to be connected to this node, then the current direction will not be changed. Otherwise, both relative left and
    * relative right directions will be checked for connected nodes and the one which has a connected node will be
    * selected. If there are no valid connected nodes in all of the checked directions, current direction will be
    * returned.
    *
    * @param map map that is being walked on.
    * @param currentPosition current position on the map.
    * @param currentDirection current movement direction on the map.
    * @return New walking direction on the map.
    */
  private def calculateNewLetterDirection(
    map:              ParsedMap,
    currentPosition:  MapPosition,
    currentDirection: Direction
  ): Direction = {
    val ahead = checkForConnectedNodeInDirection(map, currentPosition, currentDirection)

    ahead.getOrElse(calculateNewTurnDirection(map, currentPosition, currentDirection))
  }

  /**
    * Checks if provided direction has a valid connected node connected to the current node.
    *
    * @param map  map that is being walked on.
    * @param position current position on the map.
    * @param direction direction in which to get the connected node.
    * @return Specified direction wrapped in [[Some]] if there is a connected node, [[None]] otherwise.
    */
  private def checkForConnectedNodeInDirection(
    map:       ParsedMap,
    position:  MapPosition,
    direction: Direction
  ): Option[Direction] = {
    val nonConnectedNodeType =
      if direction == Direction.Up || direction == Direction.Down
      then MapNode.VerticalLine
      else MapNode.HorizontalLine

    val node = map(position.inDirection(direction))

    if node == MapNode.EmptySpace || node == nonConnectedNodeType then None else Some(direction)
  }
}
