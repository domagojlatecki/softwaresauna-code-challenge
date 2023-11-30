package at.doml.service

import at.doml.model.{ MapNode, VisitedMapNode }
import cats.effect.Sync
import cats.implicits.*

/**
  * Service which converts list of visited nodes into a string.
  *
  * @tparam F effect wrapper type.
  */
trait VisitedNodeListToStringServiceAlgebra[F[_]] {

  /**
    * Conterts list of visited nodes into a string.
    *
    * @param visitedNodes list of visited nodes.
    * @return List of visited nodes transformed into a string.
    */
  def collectVisitedNodesToString(visitedNodes: List[VisitedMapNode]): F[String]
}

/**
  * Service which transforms list of visited nodes into a string of uniquely positioned letters.
  *
  * @param F [[Sync]] typeclass instance for the used effect wrapper.
  * @tparam F effect wrapper type.
  */
class VisitedNodeListToUniquelyPositionedLettersStringServiceInterpreter[F[_]](using F: Sync[F])
    extends VisitedNodeListToStringServiceAlgebra[F] {

  override def collectVisitedNodesToString(visitedNodes: List[VisitedMapNode]): F[String] =
    for {
      lettersWithPositions      <- F.pure(
                                     visitedNodes.flatMap {
                                       case VisitedMapNode(MapNode.Letter(l), position) => Some((l, position))
                                       case _                                           => None
                                     }
                                   )
      uniquelyPositionedLetters <- F.pure(lettersWithPositions.distinct)
      uniqueLetters             <- F.pure(uniquelyPositionedLetters.map(_._1))
    } yield s"Letters: ${uniqueLetters.mkString}"
}

/**
  * Service which transforms list of visited nodes into a string of walked path characters.
  *
  * @param F [[Sync]] typeclass instance for the used effect wrapper.
  * @tparam F effect wrapper type.
  */
class VisitedNodesListToWalkedPathStringServiceInterpreter[F[_]](using F: Sync[F])
    extends VisitedNodeListToStringServiceAlgebra[F] {

  override def collectVisitedNodesToString(visitedNodes: List[VisitedMapNode]): F[String] =
    for {
      nodeCharacters <- F.pure(visitedNodes.map(_.node.char))
    } yield s"Path as characters: ${nodeCharacters.mkString}"
}
