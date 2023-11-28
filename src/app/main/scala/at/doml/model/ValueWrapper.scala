package at.doml.model

/**
  *  Template trait for
  * [[https://docs.scala-lang.org/scala3/reference/other-new-features/opaques.html Opaque Type Aliases]]. Using a trait
  * like this allows us to type wrapping reuse logic. The type wrapper is defined as a subtype of the wrapped type.
  *
  * <br>Example:
  * {{{
  * object Age extends ValueWrapper[Int]
  * type Age = Age.Type
  *
  * val age: Age = Age(30) // runtime type is Int, compile-time type is Age
  * val asInt: Int = age // compiles because Age <:< Int
  *
  * def validateAge(age: Age) = age > 18
  *
  * validateAge(Age(20)) // ok, returns true
  * validateAge(20) // compile error
  * }}}
  */
trait ValueWrapper[Wrapped] {
  opaque type Type <: Wrapped = Wrapped

  def apply(w: Wrapped): Type = w
}
