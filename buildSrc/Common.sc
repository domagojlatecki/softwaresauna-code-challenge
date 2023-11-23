import mill.define.Task
import mill.scalalib.DepSyntax
import mill.scalalib.TestModule.ScalaTest
import mill.scalalib.scalafmt.ScalafmtModule
import mill.{ Agg, T }

import $ivy.`com.lihaoyi::mill-contrib-bloop:`
import mill.contrib.bloop.Bloop

import $ivy.`com.lihaoyi::mill-contrib-scoverage:`
import mill.contrib.scoverage.ScoverageModule

import $file.Config, Config.{ Settings, Versions }

implicit class SeqTaskOps[A](seq: Seq[A]) {
  def |> [B](f: A => Task[B]) = T.sequence(seq.map(f))
}

trait ScalaModule extends mill.scalalib.ScalaModule with ScalafmtModule with ScoverageModule with Bloop.Module {
  outer =>

  private def path = {
    val segments = super.millSourcePath.segments.toList
    os.root / segments.init / "src" / segments.last
  }

  override def scalaVersion       = T(Versions.Compiler.Scala)
  override def scoverageVersion   = T(Versions.Tools.Scoverage)
  override def scalacOptions      = T(Settings.Compiler.ScalacOptions)
  override def millSourcePath     = path / "main"
  override def intellijModulePath = millSourcePath
  override def sources            = T.sources(millSourcePath / "scala")
  override def resources          = T.sources(millSourcePath / "resources")

  trait Tests extends ScalaTest with ScalafmtModule with ScoverageTests with Product {
    override def millSourcePath       = outer.path / productPrefix
    override def intellijModulePath   = millSourcePath
    override def sources              = T.sources(millSourcePath / "scala")
    override def resources            = T.sources(millSourcePath / "resources")
    override def defaultCommandName() = "test"

    override def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:${Versions.Dependencies.Test.ScalaTest}",
      ivy"org.typelevel::cats-effect-testing-scalatest::${Versions.Dependencies.Test.CatsEffectTesting}"
    )
  }

  case object unitTest extends Tests

  case object integTest extends Tests {
    override def moduleDeps = Seq(unitTest)
  }

  case object apiTest extends Tests {
    override def moduleDeps = Seq(integTest)
  }

  def reformatTests() = T.command {
    Seq[ScalafmtModule](unitTest, integTest, apiTest) |> { _.reformat() }
  }

  def reformatAll() = T.command {
    Seq[ScalafmtModule](this, unitTest, integTest, apiTest) |> { _.reformat() }
  }

  def test() = T.command {
    Seq[Tests](unitTest, integTest, apiTest) |> { _.test() }
  }

  def coverage() = T.command {
    T.sequence(
      Seq(test().map(_ => ()), scoverage.htmlReport(), scoverage.consoleReport())
    )
  }
}
