package at.doml.validators

class NoLetterForksUnparsedInputValidatorUnitSpec extends AbstractValidatorUnitSpec {

  override def validator = new NoLetterForksPathUnparsedInputValidator

  "NoLetterForksUnparsedInputValidator" - {

    fn"validate(UnparsedInput)" - {

      "should return Valid(input)" - {

        "when input is empty" in validInput("")

        "when input contains only one letter character" in validInput("A")

        "when input contains only one letter character with disjoint paths" in validInput(
          " -",
          "|A|",
          " -"
        )

        "when input has no letter turns" - {
          "in a single line" in validInput("@---x")
          "across multiple lines" in validInput("@---x", "xxx", "@@")
        }

        "when input contains one valid letter turn" - {

          "left and up" in validInput(
            " |",
            "-A"
          )

          "right and up" in validInput(
            "|",
            "A-"
          )

          "left and down" in validInput(
            "-A",
            " |"
          )

          "right and down" in validInput(
            "A-",
            "|"
          )

          "with disjoint paths" - {

            "left and up" in validInput(
              " |",
              "-A|",
              " -"
            )

            "right and up" in validInput(
              " |",
              "|A-",
              " -"
            )

            "left and down" in validInput(
              " -",
              "-A|",
              " |"
            )

            "right and down" in validInput(
              " -",
              "|A-",
              " |"
            )
          }

          "with letters" - {
            "left and up" in validInput(
              " A",
              "AA"
            )

            "right and up" in validInput(
              "A",
              "AA"
            )

            "left and down" in validInput(
              "AA",
              " A"
            )

            "right and down" in validInput(
              "AA",
              "A"
            )
          }
        }

        "when input contains a dead-end with a letter character" - {

          "with path left" in validInput("-A")

          "with path right" in validInput("A-")

          "with path up" in validInput(
            "|",
            "A"
          )

          "with path down" in validInput(
            "A",
            "|"
          )

          "with letter left" in validInput("BA")

          "with letter right" in validInput("AB")

          "with letter up" in validInput(
            "B",
            "A"
          )

          "with letter down" in validInput(
            "A",
            "B"
          )

          "with disjoint paths" - {

            "with path left" in validInput(
              " -",
              "-A|",
              " -"
            )

            "with path right" in validInput(
              " -",
              "|A-",
              " -"
            )

            "with path up" in validInput(
              " |",
              "|A|",
              " -"
            )

            "with path down" in validInput(
              " -",
              "|A|",
              " |"
            )
          }
        }

        "when input contains a letter in the straight path" - {

          "left and right" in validInput("-A-")

          "up and down" in validInput(
            "|",
            "A",
            "|"
          )

          "with disjoint paths" - {

            "left and right" in validInput(
              " -",
              "-A-",
              " -"
            )

            "up and down" in validInput(
              " |",
              "|A|",
              " |"
            )
          }

          "with letters" - {
            "left and right" in validInput("AAA")

            "up and down" in validInput(
              "A",
              "A",
              "A"
            )
          }
        }

        "when input has letter crossing" in validInput(
          " |",
          "-A-",
          " |"
        )

        "when input contains multiple valid letter turns" in validInput(
          "-----A",
          "G-F  B---C",
          "| |      |",
          "| E------D",
          "H---------"
        )

        "when input contains a square of letter turns" in validInput(
          "AB",
          "CD"
        )

        "when input has compact letter turns" in validInput(
          " G-L-H",
          " |  BA-J",
          "@BC AA H",
          " ED    x"
        )
      }

      "should return Invalid(errorMessages)" - {

        "when input contains a fork in the path" - {
          "left" in forkInPath(
            " |",
            " A-",
            " |"
          )(line = 2, column = 2)

          "right" in forkInPath(
            " |",
            "-A",
            " |"
          )(line = 2, column = 2)

          "up" in forkInPath(
            "",
            "-A-",
            " |"
          )(line = 2, column = 2)

          "down" in forkInPath(
            " |",
            "-A-",
            ""
          )(line = 2, column = 2)

          "with disjoint path" - {
            "left" in forkInPath(
              " |",
              "|A-",
              " |"
            )(line = 2, column = 2)

            "right" in forkInPath(
              " |",
              "-A|",
              " |"
            )(line = 2, column = 2)

            "up" in forkInPath(
              " -",
              "-A-",
              " |"
            )(line = 2, column = 2)

            "down" in forkInPath(
              " |",
              "-A-",
              " -"
            )(line = 2, column = 2)
          }
        }

        "when input contains multiple forks in letter paths" in multiErrorInvalidInput(
          "AAA",
          "AAA",
          "AAA"
        )(
          forkInPathError(line = 1, column = 2),
          forkInPathError(line = 2, column = 1),
          forkInPathError(line = 2, column = 3),
          forkInPathError(line = 3, column = 2)
        )
      }
    }
  }
}
