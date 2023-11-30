package at.doml

import at.doml.model.{ ParsedMap, ProgramOutput }
import at.doml.service.*
import cats.effect.Sync
import cats.implicits.*
import cats.syntax.traverse.toTraverseOps

/**
  * Module which contains logic for map processing and returning program output.
  *
  * @param mapWalkerService map walker service instance.
  * @param visitedNodeListToStringServices list of visited node to string services.
  * @param F [[Sync]] typeclass instance for the used effect wrapper.
  * @tparam F effect wrapper type.
  */
class MapProcessingModule[F[_]](
  using
  mapWalkerService:                MapWalkerServiceAlgebra[F],
  visitedNodeListToStringServices: List[VisitedNodeListToStringServiceAlgebra[F]],
  F:                               Sync[F]
) {

  /**
    * Processes the parsed input map and returns program output value.
    *
    * @param map parsed map to process.
    * @param maxSteps maximum number of steps while processing the map.
    * @return Program output value, or error if map cannot be processed.
    */
  def processMap(map: ParsedMap, maxSteps: Int): F[ProgramOutput] =
    for {
      visitedMapNodes <- mapWalkerService.walkOnMap(map, maxSteps)
      outputStrings   <- visitedNodeListToStringServices.traverse(_.collectVisitedNodesToString(visitedMapNodes))
    } yield ProgramOutput(outputStrings.mkString("\n"))
}

/**
  * Companion object which contains initialization logic for [[MapProcessingModule]].
  */
object MapProcessingModule {

  /**
    * Create an instance of [[MapProcessingModule]] which process the map and returns program output.
    *
    * @tparam F effect wrapper type - needs to have a [[Sync]] instance implicitly available.
    * @return Module which can process the parsed map and return program output.
    */
  def create[F[_] : Sync]: F[MapProcessingModule[F]] =
    Sync[F].pure {
      given MapWalkerServiceAlgebra[F]                     = new MapWalkerServiceInterpreter[F]
      given List[VisitedNodeListToStringServiceAlgebra[F]] = List(
        new VisitedNodeListToUniquelyPositionedLettersStringServiceInterpreter[F],
        new VisitedNodesListToWalkedPathStringServiceInterpreter[F]
      )

      new MapProcessingModule[F]
    }
}
