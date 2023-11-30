package at.doml.error

/**
  * Error which indicates that parsing error has occured.
  *
  * @param msg error message.
  */
case class ParsingError(msg: String) extends AppError(msg)
