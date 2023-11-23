package test

import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.EitherValues
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

abstract class UnitSpec extends AsyncFreeSpec, AsyncIOSpec, Matchers, EitherValues {
  extension (sc: StringContext) def fn(args: Any*): String = s"[ ${sc.s(args: _*)} ]"
}
