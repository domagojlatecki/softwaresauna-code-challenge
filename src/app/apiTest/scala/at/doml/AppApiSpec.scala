package at.doml

import cats.effect.{ ExitCode, IO }
import test.{ ApiSpec, TrackedPrintStream }

class AppApiSpec extends ApiSpec {

  "App" - {

    val testFilesFolderPath = getClass.getClassLoader.getResource("app-testing").getPath

    def app(args: String*)(expectedExitCode: ExitCode, expectedOutputLines: String*) = {
      val result = for {
        outputTracker <- IO.pure(new TrackedPrintStream)
        _             <- IO.delay(System.setOut(outputTracker))
        exitCode      <- App.run(args.toList)
        outputLines   <- IO.delay(outputTracker.capturedOutput)
      } yield (exitCode, outputLines)

      result.asserting(_ shouldBe (expectedExitCode, expectedOutputLines.mkString(start = "", sep = "\n", end = "\n")))
    }

    "must return success status and print correct output for valid input map" - {

      "for a basic example" in app(s"$testFilesFolderPath/valid/basic-example.txt")(
        ExitCode.Success,
        "Letters: ACB",
        "Path as characters: @---A---+|C|+---+|+-B-x"
      )

      "for a basic example with --max-steps 100" in app(
        "--max-steps",
        "100",
        s"$testFilesFolderPath/valid/basic-example.txt"
      )(
        ExitCode.Success,
        "Letters: ACB",
        "Path as characters: @---A---+|C|+---+|+-B-x"
      )

      "when going straight through intersections" in app(
        s"$testFilesFolderPath/valid/go-straight-through-intersections.txt"
      )(
        ExitCode.Success,
        "Letters: ABCD",
        "Path as characters: @|A+---B--+|+--C-+|-||+---D--+|x"
      )

      "when letter is on a turn" in app(s"$testFilesFolderPath/valid/letters-on-turns.txt")(
        ExitCode.Success,
        "Letters: ACB",
        "Path as characters: @---A---+|||C---+|+-B-x"
      )

      "without collecting a letter from the same location twice" in app(
        s"$testFilesFolderPath/valid/do-not-collect-letter-from-same-location-twice.txt"
      )(
        ExitCode.Success,
        "Letters: GOONIES",
        "Path as characters: @-G-O-+|+-+|O||+-O-N-+|I|+-+|+-I-+|ES|x"
      )

      "while keeping direction in compact space" in app(
        s"$testFilesFolderPath/valid/keep-direction-in-compact-space.txt"
      )(
        ExitCode.Success,
        "Letters: BLAH",
        "Path as characters: @B+++B|+-L-+A+++A-+Hx"
      )

      "while ignoring stuff after end of path" in app(s"$testFilesFolderPath/valid/ignore-stuff-after-end-of-path.txt")(
        ExitCode.Success,
        "Letters: AB",
        "Path as characters: @-A--+|+-B--x"
      )
    }

    "must return error status and print correct output for invalid program arguments" - {

      "when no program arguments are specified" in app()(
        ExitCode.Error,
        "Usage: [--max-steps <n>] inputFile"
      )

      "when non-existent file path is specified" in app(s"$testFilesFolderPath/non-existent")(
        ExitCode.Error,
        "Error:",
        "Could not load data from provided file"
      )

      "when --max-steps argument is missing a value" in app("--max-steps")(
        ExitCode.Error,
        "Missing value for --max-steps argument"
      )

      "when file path is missing after --max-steps argument" in app("--max-steps", "100")(
        ExitCode.Error,
        "Usage: [--max-steps <n>] inputFile"
      )

      "when no program arguments are incorrect" in app("--incorrect", "arg")(
        ExitCode.Error,
        "Usage: [--max-steps <n>] inputFile"
      )

      "when --max-steps has a non-number value" in app("--max-steps", "n", "src")(
        ExitCode.Error,
        "Argument for --max-steps must be a whole number"
      )

      "when there are too many arguments" in app("--max-steps", "n", "src", "more", "args")(
        ExitCode.Error,
        "Usage: [--max-steps <n>] inputFile"
      )
    }

    "must return error status and print correct output for invalid input map" - {

      "for input with missing start character" in app(s"$testFilesFolderPath/invalid/missing-start-character.txt")(
        ExitCode.Error,
        "Input error(s):",
        "Missing start character ('@') in input"
      )

      "for input with missing end character" in app(s"$testFilesFolderPath/invalid/missing-end-character.txt")(
        ExitCode.Error,
        "Input error(s):",
        "Missing end character ('x') in input"
      )

      "for input with multiple start characters" - {

        "example 1" in app(s"$testFilesFolderPath/invalid/multiple-starts-1.txt")(
          ExitCode.Error,
          "Input error(s):",
          "Only one start character ('@') is expected in input, but 2 were found",
          "Starting node has more than one path at line: 1, column: 9"
        )

        "example 2" in app(s"$testFilesFolderPath/invalid/multiple-starts-2.txt")(
          ExitCode.Error,
          "Input error(s):",
          "Only one start character ('@') is expected in input, but 2 were found"
        )

        "example 3" in app(s"$testFilesFolderPath/invalid/multiple-starts-3.txt")(
          ExitCode.Error,
          "Input error(s):",
          "Only one start character ('@') is expected in input, but 2 were found"
        )
      }

      "for input with fork in path" in app(s"$testFilesFolderPath/invalid/fork-in-path.txt")(
        ExitCode.Error,
        "Input error(s):",
        "Fork in path at line: 3, column: 11"
      )

      "for input with letter fork in path" in app(s"$testFilesFolderPath/invalid/letter-fork-in-path.txt")(
        ExitCode.Error,
        "Input error(s):",
        "Fork in path at line: 3, column: 11"
      )

      "for input with broken path" in app(s"$testFilesFolderPath/invalid/broken-path.txt")(
        ExitCode.Error,
        "Error:",
        "Empty space was encountered - map has a broken path"
      )

      "for input with multiple starting paths" in app(s"$testFilesFolderPath/invalid/multiple-starting-paths.txt")(
        ExitCode.Error,
        "Input error(s):",
        "Starting node has more than one path at line: 1, column: 7"
      )

      "for input with fake turn" in app(s"$testFilesFolderPath/invalid/fake-turn.txt")(
        ExitCode.Error,
        "Input error(s):",
        "Fake turn at line: 1, column: 7"
      )

      "for input with non-allowed character" in app(s"$testFilesFolderPath/invalid/non-allowed-character.txt")(
        ExitCode.Error,
        "Input error(s):",
        "Invalid character(s) in input: 'a'"
      )

      "when there are not enough steps to finish walking the map" in app(
        "--max-steps",
        "10",
        s"$testFilesFolderPath/valid/basic-example.txt"
      )(
        ExitCode.Error,
        "Error:",
        "Could not reach the end of the map in specified number of steps"
      )
    }
  }
}
