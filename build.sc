import mill.scalalib.DepSyntax
import mill.{ Agg, T }

import $file.buildSrc.Common, Common.{ ScalaModule, SeqTaskOps }
import $file.buildSrc.Config, Config.{ Settings, Versions }

/* * * * * *
 * Modules *
 * * * * * */

object app extends ScalaModule {

  override def mainClass = T(Some(Settings.Project.MainClass))

  override def ivyDeps = Agg(
    ivy"org.typelevel::cats-core:${Versions.Dependencies.Cats}",
    ivy"org.typelevel::cats-kernel:${Versions.Dependencies.Cats}",
    ivy"org.typelevel::cats-effect:${Versions.Dependencies.CatsEffect}",
    ivy"org.typelevel::cats-effect-std:${Versions.Dependencies.CatsEffect}",
    ivy"org.typelevel::cats-effect-kernel:${Versions.Dependencies.CatsEffect}",
  )
}

val allModules = Seq[ScalaModule](app)

/* * * * *
 * Tasks *
 * * * * */

def compile() = T.command {
  allModules |> { _.compile }
}

def cc(ev: mill.eval.Evaluator) = T.command {
  T.sequence(
    Seq(
      clean(ev).map(_ => ()),
      compile().map(_ => ())
    )
  )
}

def reformat() = T.command {
  allModules |> { _.reformatAll() }
}

def unitTest() = T.command {
  allModules |> { _.unitTest.test() }
}

def integTest() = T.command {
  allModules |> { _.integTest.test() }
}

def apiTest() = T.command {
  allModules |> { _.apiTest.test() }
}

def test() = T.command {
  allModules |> { _.test() }
}

def coverage() = T.command {
  allModules |> { _.coverage() }
}
