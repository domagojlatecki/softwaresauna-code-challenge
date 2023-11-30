package at.doml.error

/**
  * Error which indicates that parsing has failed.
  *
  * @param message error message.
  */
case class ParsingError(message: String) extends AppError(message)
