package at.doml.validators

/**
  * Validator that checks if input contains specified character exactly once.
  *
  * @param char character to check for.
  * @param charName name of the character to show in the validation error message.
  */
class ExactlyOneCharacterUnparsedInputValidator(char: Char, charName: String)
    extends AbstractCharacterCountValidator(char) {

  protected override def validateCount(count: Int): Option[String] =
    count match {
      case 1 => None
      case 0 => Some(s"Missing $charName character ('$char') in input")
      case n => Some(s"Only one $charName character ('$char') is expected in input, but $n were found")
    }
}
