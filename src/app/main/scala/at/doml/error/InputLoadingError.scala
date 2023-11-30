package at.doml.error

/**
  * Error which indicates that input loading has failed.
  *
  * @param message error message.
  * @param cause optional cause of this error.
  */
case class InputLoadingError(message: String, cause: Option[Throwable]) extends AppError(s"Error:\n$message", cause)
