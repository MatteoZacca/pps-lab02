package tasks

// overall module
object Optionals:

  // type "public" definition, exposing structure
  enum OptionalInt:
    case Maybe(value: Int)
    case Empty()

  // operations (/algorithms)
  object OptionalInt:

    def isEmpty(opt: OptionalInt): Boolean = opt match
      case Empty() => true
      case _       => false

    def orElse(opt: OptionalInt, orElse: Int): Int = opt match
      case Maybe(a) => a
      case _       => orElse

@main def tryOptionals(): Unit =
  import Optionals.*
  import OptionalInt.* // to directly access algorithms

  val s1: OptionalInt = Maybe(1)
  val s2: OptionalInt = Empty()

  println(s1) // Some(1)
  println(isEmpty(s1)) // false
  println(orElse(s1, 0)) // 1
  println(orElse(s2, 0)) // 0
