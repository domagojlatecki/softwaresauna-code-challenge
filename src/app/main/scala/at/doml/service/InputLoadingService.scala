package at.doml.service

import java.nio.charset.StandardCharsets
import java.nio.file.{ Path, Paths }
import at.doml.error.InputLoadingError
import at.doml.model.{ InputSource, UnparsedInput }
import cats.effect.{ Resource, Sync }
import cats.implicits.*
import scala.collection.immutable.ArraySeq
import scala.io.Source

/**
  * Service used to load program input.
  *
  * @tparam F effect wrapper type.
  */
trait InputLoadingServiceAlgebra[F[_]] {

  /**
    * Loads program input from provided source string.
    *
    * @param source source of the input.
    * @return Loaded unparsed program input.
    */
  def load(source: InputSource): F[UnparsedInput]
}

/**
  * Implementation of the service used to load program input from file.
  *
  * @param F [[Sync]] typeclass instance for the used effect wrapper.
  * @tparam F effect wrapper type.
  */
class FileInputLoadingServiceInterpreter[F[_]](using F: Sync[F]) extends InputLoadingServiceAlgebra[F] {

  override def load(source: InputSource): F[UnparsedInput] =
    for {
      path      <- F.delay(Paths.get(source)).adaptError(e => InputLoadingError("Input file path is invalid", Some(e)))
      fileLines <- loadFileData(path)
    } yield UnparsedInput(fileLines)

  private def loadFileData(path: Path): F[ArraySeq[String]] =
    Resource.make(acquire = F.delay(Source.fromFile(path.toFile, StandardCharsets.UTF_8.name)))( //
      release = source => F.delay(source.close())
    )
      .use(source => F.delay(source.getLines().to(ArraySeq)))
      .adaptError(e => InputLoadingError("Could not load data from provided file", Some(e)))
}
