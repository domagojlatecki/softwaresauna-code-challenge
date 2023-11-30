package at.doml.service

import at.doml.error.ParsingError
import at.doml.model.{ MapNode, MapPosition, ParsedMap, UnparsedInput }
import cats.data.Validated
import cats.effect.Sync
import cats.implicits.*
import scala.collection.immutable.ArraySeq

/**
  * Service used to parse program input.
  *
  * @tparam F effect wrapper type.
  */
trait MapParsingServiceAlgebra[F[_]] {

  /**
    * Parses the provided raw program input into [[ParsedMap]] and raises an error if input cannot be parsed.
    *
    * @param input input to parse.
    * @return Input parsed into [[ParsedMap]] object or error if input cannot be parsed.
    */
  def parse(input: UnparsedInput): F[ParsedMap]
}

/**
  * Implementation of the service used to parse program input.
  *
  * @param F [[Sync]] typeclass instance for the used effect wrapper.
  * @tparam F effect wrapper type.
  */
class MapParsingServiceInterpreter[F[_]](using F: Sync[F]) extends MapParsingServiceAlgebra[F] {

  override def parse(input: UnparsedInput): F[ParsedMap] =
    for {
      mapNodes         <- F.pure(input.map(parseLine))
      optStartPosition <- F.pure(findStartPosition(mapNodes))
      startPosition    <- F.fromOption(optStartPosition, ParsingError("Start node not found on the map"))
    } yield ParsedMap(mapNodes, startPosition)

  private def parseLine(line: String): ArraySeq[MapNode] =
    line.map(MapNode.fromChar).to(ArraySeq)

  private def findStartPosition(mapNodes: ArraySeq[ArraySeq[MapNode]]): Option[MapPosition] = {
    val nodesWithPositions: Iterator[(MapNode, MapPosition)] =
      mapNodes.iterator.zipWithIndex.flatMap { case (line, y) =>
        line.iterator.zipWithIndex.map { case (node, x) => (node, MapPosition(x, y)) }
      }

    val optStartNode: Option[(MapNode, MapPosition)] =
      nodesWithPositions.find { case (node, _) => node == MapNode.Start }

    optStartNode.map { case (_, position) => position }
  }
}
