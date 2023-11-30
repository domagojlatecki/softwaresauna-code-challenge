package at.doml

import at.doml.error.MapWalkingError
import at.doml.model.{ MapPosition, ParsedMap, VisitedMapNode }
import at.doml.service.{ MapWalkerServiceAlgebra, VisitedNodeListToStringServiceAlgebra }
import cats.effect.IO
import test.UnitSpec
import scala.collection.immutable.ArraySeq

class MapProcessingModuleUnitSpec extends UnitSpec {

  "MapProcessingModule" - {

    val map      = ParsedMap(ArraySeq.empty, MapPosition(0, 0))
    val maxSteps = 0

    given MapWalkerServiceAlgebra[IO] = new MapWalkerServiceAlgebra[IO] {
      override def walkOnMap(map: ParsedMap, maxSteps: Int): IO[List[VisitedMapNode]] =
        IO(Nil)
    }

    given List[VisitedNodeListToStringServiceAlgebra[IO]] =
      new VisitedNodeListToStringServiceAlgebra[IO] {
        override def collectVisitedNodesToString(visitedNodes: List[VisitedMapNode]): IO[String] =
          IO("")
      } :: Nil

    fn"processMap(ParsedMap, Int)" - {

      "should correctly process map" in {
        val module = new MapProcessingModule[IO]

        module.processMap(map, maxSteps)
          .asserting(_ shouldBe "")
      }

      "should raise error" - {

        "when map walking fails" in {
          val module = {
            given MapWalkerServiceAlgebra[IO] = new MapWalkerServiceAlgebra[IO] {
              override def walkOnMap(map: ParsedMap, maxSteps: Int): IO[List[VisitedMapNode]] =
                IO.raiseError(MapWalkingError(""))
            }

            new MapProcessingModule[IO]
          }

          module.processMap(map, maxSteps)
            .assertThrowsError(_ shouldBe a[MapWalkingError])
        }
      }
    }
  }
}
