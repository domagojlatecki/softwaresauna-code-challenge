package at.doml.error

import at.doml.model.ErrorMessage

/**
  * Error which indicates that input validation has failed.
  *
  * @param errors list of validation errors.
  */
case class ValidationError(errors: List[ErrorMessage])
    extends AppError(errors.mkString(start = "Input error(s):\n", sep = "\n", end = ""))
