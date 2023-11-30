package at.doml.service

import at.doml.error.InputLoadingError
import at.doml.model.{ InputSource, UnparsedInput }
import cats.effect.IO
import test.UnitSpec
import scala.collection.immutable.ArraySeq

class FileInputLoadingServiceInterpreterUnitSpec extends UnitSpec {

  "FileInputLoadingServiceInterpreter" - {

    val testFilesFolderPath = getClass.getClassLoader.getResource("file-loading-tests").getPath
    val service             = new FileInputLoadingServiceInterpreter[IO]

    fn"load(String)" - {

      "should correctly load data from some file" in {
        service.load(InputSource(s"$testFilesFolderPath/valid-file.txt"))
          .asserting(
            _ shouldBe UnparsedInput(
              ArraySeq(
                "@--+",
                "   |",
                " x-+"
              )
            )
          )
      }

      "should raise InputLoadingError" - {
        def expectedInputLoadingError(fileName: String) =
          service.load(InputSource(fileName))
            .assertThrowsError {
              case InputLoadingError(message, _) => message shouldBe "Could not load data from provided file"
              case other                         => fail(s"Expected InputLoadingError, but got: ${other.getClass}")
            }

        "when input path is empty" in expectedInputLoadingError("")

        "when input file does not exist" in expectedInputLoadingError(s"$testFilesFolderPath/non-existent-file")

        "when input file is a folder" in expectedInputLoadingError(s"$testFilesFolderPath/folder")
      }
    }
  }
}
