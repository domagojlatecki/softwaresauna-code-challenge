import mill.scalalib.DepSyntax
import mill.Agg

object Versions {
  val Project = "0.0.1"

  object Compiler {
    val Scala = "3.3.1"
  }

  object Tools {
    val Scoverage = "2.0.11"
  }

  object Dependencies {
    val Cats            = "2.10.0"
    val CatsEffect      = "3.5.2"

    object Test {
      val ScalaTest         = "3.2.17"
      val CatsEffectTesting = "1.5.0"
    }
  }
}

object Settings {

  object Project {
    val MainClass = "at.doml.App"
  }

  object Compiler {
    private val LineWidth = 120
    private val JvmTarget = 17

    val ScalacOptions = Seq(
      // Standard settings
      "-encoding:utf-8",
      s"-pagewidth:$LineWidth",
      s"-release:$JvmTarget",
      "-deprecation",
      "-new-syntax",
      "-no-indent",
      "-feature",
      "-unchecked",
      "-Wunused:all",
      "-Wvalue-discard",
      // Advanced settings
      "-Xcheck-macros",
      // Private settings
      "-Ysafe-init"
    )
  }
}
