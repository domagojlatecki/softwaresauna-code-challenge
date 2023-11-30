package at.doml.error

/**
  * Top-level error type for all errors raised in this application.
  *
  * @param message error message.
  * @param cause optional cause of this error.
  */
abstract class AppError(
  message: String,
  cause:   Option[Throwable] = None
) extends Exception(message, cause.orNull)
