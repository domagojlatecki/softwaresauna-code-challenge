package at.doml

import at.doml.model.{ InputSource, MapNode, ParsedMap }
import at.doml.service.*
import at.doml.validators.*
import cats.effect.Sync
import cats.implicits.*

/**
  * Module which contains logic for program input processing, including loading the input map from file, validating the
  * input and parsing the input into a [[ParsedMap]] object.
  *
  * @param inputLoadingService input loading service instance.
  * @param validationServiceAlgebra validation service instance.
  * @param mapParsingServiceAlgebra map parsing service instance.
  * @param F [[Sync]] typeclass instance for the used effect wrapper.
  * @tparam F effect wrapper type.
  */
class InputProcessingModule[F[_]](
  using
  inputLoadingService:      InputLoadingServiceAlgebra[F],
  validationServiceAlgebra: ValidationServiceAlgebra[F],
  mapParsingServiceAlgebra: MapParsingServiceAlgebra[F],
  F:                        Sync[F]
) {

  /**
    * Loads, validates and parses the map from provided input source.
    *
    * @param source source of the input.
    * @return Data loaded from source parsed into [[ParsedMap]] object or error if input cannot read or parsed, or if it
    *         is not valid.
    */
  def loadMap(source: InputSource): F[ParsedMap] =
    for {
      unparsedInput  <- inputLoadingService.load(source)
      validatedInput <- validationServiceAlgebra.validate(unparsedInput)
      parsedMap      <- mapParsingServiceAlgebra.parse(validatedInput)
    } yield parsedMap
}

/**
  * Companion object which contains initialization logic for [[InputProcessingModule]].
  */
object InputProcessingModule {

  /**
    * Create an instance of [[InputProcessingModule]] which will load input from file, validate it and parse the map.
    *
    * @tparam F effect wrapper type - needs to have a [[Sync]] instance implicitly available.
    * @return Module which can load and process input from file path.
    */
  def create[F[_] : Sync]: F[InputProcessingModule[F]] =
    Sync[F].pure {
      val allowedCharacters = MapNode.NonLetterCharacters ++ ('A' to 'Z')

      given List[UnparsedInputValidator] = List(
        new ExactlyOneCharacterUnparsedInputValidator(MapNode.Start.char, "start"),
        new AtLeastOneCharacterUnparsedInputValidator(MapNode.End.char, "end"),
        new AllowedCharactersUnparsedInputValidator(allowedCharacters),
        new ValidTurnsUnparsedInputValidator,
        new NoLetterForksPathUnparsedInputValidator,
        new OneStartingPathUnparsedInputValidator
      )

      given InputLoadingServiceAlgebra[F] = new FileInputLoadingServiceInterpreter[F]
      given ValidationServiceAlgebra[F]   = new ErrorAccumulatingValidationServiceInterpreter[F]
      given MapParsingServiceAlgebra[F]   = new MapParsingServiceInterpreter[F]

      new InputProcessingModule[F]
    }
}
