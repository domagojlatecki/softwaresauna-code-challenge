package at.doml.validators

class ValidTurnsUnparsedInputValidatorUnitSpec extends AbstractValidatorUnitSpec {

  override def validator = new ValidTurnsUnparsedInputValidator

  "ValidTurnsUnparsedInputValidator" - {

    fn"validate(UnparsedInput)" - {

      "should return Valid(input)" - {

        "when input is empty" in validInput("")

        "when input has no turns" - {
          "in a single line" in validInput("@---x")
          "across multiple lines" in validInput("@---x", "ABC", "@@")
        }

        "when input contains one valid turn" - {

          "left and up" in validInput(
            " |",
            "-+"
          )

          "right and up" in validInput(
            "|",
            "+-"
          )

          "left and down" in validInput(
            "-+",
            " |"
          )

          "right and down" in validInput(
            "+-",
            "|"
          )

          "with disjoint paths" - {

            "left and up" in validInput(
              " |",
              "-+|",
              " -"
            )

            "right and up" in validInput(
              " |",
              "|+-",
              " -"
            )

            "left and down" in validInput(
              " -",
              "-+|",
              " |"
            )

            "right and down" in validInput(
              " -",
              "|+-",
              " |"
            )
          }

          "with letters" - {
            "left and up" in validInput(
              " A",
              "A+"
            )

            "right and up" in validInput(
              "A",
              "+A"
            )

            "left and down" in validInput(
              "A+",
              " A"
            )

            "right and down" in validInput(
              "+A",
              "A"
            )
          }
        }

        "when input contains multiple valid turns" in validInput(
          "-----+",
          "+-+  +---+",
          "| |      |",
          "| +------+",
          "+---------"
        )

        "when input contains a square of turns" in validInput(
          "++",
          "++"
        )

        "when input has compact turns" in validInput(
          " +-L-+",
          " |  +A-+",
          "@B+ ++ H",
          " ++    x"
        )
      }

      "should return Invalid(errorMessages)" - {

        "when input contains only one turn character" in fakeTurn("+")(line = 1, column = 1)

        "when input contains only one turn character with disjoint paths" in fakeTurn(
          " -",
          "|+|",
          " -"
        )(line = 2, column = 2)

        "when input contains a dead-end fake turn" - {

          "with path left" in fakeTurn("-+")(line = 1, column = 2)

          "with path right" in fakeTurn("+-")(line = 1, column = 1)

          "with path up" in fakeTurn(
            "|",
            "+"
          )(line = 2, column = 1)

          "with path down" in fakeTurn(
            "+",
            "|"
          )(line = 1, column = 1)

          "with letter left" in fakeTurn("A+")(line = 1, column = 2)

          "with letter right" in fakeTurn("+A")(line = 1, column = 1)

          "with letter up" in fakeTurn(
            "A",
            "+"
          )(line = 2, column = 1)

          "with letter down" in fakeTurn(
            "+",
            "A"
          )(line = 1, column = 1)

          "with disjoint paths" - {

            "with path left" in fakeTurn(
              " -",
              "-+|",
              " -"
            )(line = 2, column = 2)

            "with path right" in fakeTurn(
              " -",
              "|+-",
              " -"
            )(line = 2, column = 2)

            "with path up" in fakeTurn(
              " |",
              "|+|",
              " -"
            )(line = 2, column = 2)

            "with path down" in fakeTurn(
              " -",
              "|+|",
              " |"
            )(line = 2, column = 2)
          }
        }

        "when input contains straight fake turn" - {

          "left and right" in fakeTurn("-+-")(line = 1, column = 2)

          "up and down" in fakeTurn(
            "|",
            "+",
            "|"
          )(line = 2, column = 1)

          "with disjoint paths" - {

            "left and right" in fakeTurn(
              " -",
              "-+-",
              " -"
            )(line = 2, column = 2)

            "up and down" in fakeTurn(
              " |",
              "|+|",
              " |"
            )(line = 2, column = 2)
          }

          "with letters" - {
            "left and right" in fakeTurn("A+A")(line = 1, column = 2)

            "up and down" in fakeTurn(
              "A",
              "+",
              "A"
            )(line = 2, column = 1)
          }
        }

        "when input contains a fork in the path" - {
          "left" in forkInPath(
            " |",
            " +-",
            " |"
          )(line = 2, column = 2)

          "right" in forkInPath(
            " |",
            "-+",
            " |"
          )(line = 2, column = 2)

          "up" in forkInPath(
            "",
            "-+-",
            " |"
          )(line = 2, column = 2)

          "down" in forkInPath(
            " |",
            "-+-",
            ""
          )(line = 2, column = 2)

          "full" in forkInPath(
            " |",
            "-+-",
            " |"
          )(line = 2, column = 2)

          "with disjoint path" - {
            "left" in forkInPath(
              " |",
              "|+-",
              " |"
            )(line = 2, column = 2)

            "right" in forkInPath(
              " |",
              "-+|",
              " |"
            )(line = 2, column = 2)

            "up" in forkInPath(
              " -",
              "-+-",
              " |"
            )(line = 2, column = 2)

            "down" in forkInPath(
              " |",
              "-+-",
              " -"
            )(line = 2, column = 2)
          }

          "with letters" - {
            "left" in forkInPath(
              " A",
              " +A",
              " A"
            )(line = 2, column = 2)

            "right" in forkInPath(
              " A",
              "A+",
              " A"
            )(line = 2, column = 2)

            "up" in forkInPath(
              "",
              "A+A",
              " A"
            )(line = 2, column = 2)

            "down" in forkInPath(
              " A",
              "A+A",
              ""
            )(line = 2, column = 2)

            "full" in forkInPath(
              " A",
              "A+A",
              " A"
            )(line = 2, column = 2)
          }
        }

        "when input contains multiple fake turns and forks in turns" in multiErrorInvalidInput(
          "+++  |   |",
          "+++  +  -+-",
          "+++  |   |",
          "",
          "    -+-"
        )(
          forkInPathError(line = 1, column = 2),
          forkInPathError(line = 2, column = 1),
          forkInPathError(line = 2, column = 2),
          forkInPathError(line = 2, column = 3),
          fakeTurnError(line   = 2, column = 6),
          forkInPathError(line = 2, column = 10),
          forkInPathError(line = 3, column = 2),
          fakeTurnError(line   = 5, column = 6)
        )
      }
    }
  }
}
