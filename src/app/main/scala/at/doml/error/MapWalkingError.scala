package at.doml.error

/**
  * Error which indicates that map walking process has failed.
  *
  * @param message error message.
  */
case class MapWalkingError(message: String) extends AppError(message)
