package at.doml.validators

/**
  * Validator that checks if input contains specified character at least once.
  *
  * @param char character to check for.
  * @param charName name of the character to show in the validation error message.
  */
class AtLeastOneCharacterUnparsedInputValidator(char: Char, charName: String)
    extends AbstractCharacterCountValidator(char) {

  protected override def validateCount(count: Int): Option[String] =
    count match {
      case 0 => Some(s"Missing $charName character ('$char') in input")
      case _ => None
    }
}
