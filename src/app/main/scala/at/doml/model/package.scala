package at.doml.model

import scala.collection.immutable.ArraySeq

object UnparsedInput extends ValueWrapper[ArraySeq[String]]
type UnparsedInput = UnparsedInput.Type

object ErrorMessage extends ValueWrapper[String]
type ErrorMessage = ErrorMessage.Type
