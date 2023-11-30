package at.doml

import at.doml.error.AppError
import at.doml.model.InputSource
import cats.effect.{ExitCode, IO, IOApp}

object App extends IOApp {

  case class ProgramArguments(source: InputSource, maxSteps: Int)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      parsedArgs <- parseArgs(args)
      exitCode   <- parsedArgs match {

                      case Left(ec)  =>
                        IO.pure(ec)

                      case Right(pa) =>
                        runProgram(pa)
                          .handleErrorWith {
                            case ae: AppError =>
                              IO.println(ae.getMessage) *>
                                IO.pure(ExitCode.Error)

                            case other =>
                              IO.println(s"Unexpected error") *>
                                IO.delay(other.printStackTrace()) *>
                                IO.pure(ExitCode.Error)
                          }
                    }
    } yield exitCode

  private def parseArgs(args: List[String]): IO[Either[ExitCode, ProgramArguments]] =
    args match {

      case "--max-steps" :: Nil =>
        IO.println("Missing value for --max-steps argument") *>
          IO.pure(Left(ExitCode.Error))

      case source :: Nil =>
        IO.pure(Right(ProgramArguments(InputSource(source), 10_000)))

      case "--max-steps" :: n :: source :: Nil =>
        n.toIntOption match {
          case Some(maxSteps) =>
            IO.pure(Right(ProgramArguments(InputSource(source), maxSteps)))

          case None =>
            IO.println("Argument for --max-steps must be a whole number") *>
              IO.pure(Left(ExitCode.Error))
        }

      case _ =>
        IO.println("Usage: [--max-steps <n>] inputFile") *>
          IO.pure(Left(ExitCode.Error))
    }

  private def runProgram(programArguments: ProgramArguments): IO[ExitCode] =
    for {
      inputProcessing <- InputProcessingModule.create[IO]
      loadedMap       <- inputProcessing.loadMap(programArguments.source)
      mapProcessing   <- MapProcessingModule.create[IO]
      programResult   <- mapProcessing.processMap(loadedMap, programArguments.maxSteps)
      _               <- IO.println(programResult)
    } yield ExitCode.Success
}
