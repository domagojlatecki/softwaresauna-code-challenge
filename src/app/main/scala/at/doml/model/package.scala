package at.doml.model

import scala.collection.immutable.ArraySeq

object InputSource extends ValueWrapper[String]

/**
  * Opaque type wrapper for input source: [[String]].
  */
type InputSource = InputSource.Type

object UnparsedInput extends ValueWrapper[ArraySeq[String]]

/**
  * Opaque type wrapper for unparsed input: [[ArraySeq]] of [[String]].
  */
type UnparsedInput = UnparsedInput.Type

object ErrorMessage extends ValueWrapper[String]

/**
  * Opaque type wrapper for validation error messages: [[String]].
  */
type ErrorMessage = ErrorMessage.Type
