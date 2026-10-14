package tasks

// functional compositions with 3 functions
def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D =
  x => f(g(h(x)))

// reusing generic compose
def composeThreeReusing[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D =
  composeGeneric(f, composeGeneric(g, h))

@main def Task6(): Unit =
  val f1: String => String = _ + "!"
  val g1: Int => String = _.toString
  val h1: Int => Int = _ * 2

  println(composeThree(f1, g1, h1)(3))
  println(composeThreeReusing(f1, g1, h1)(3))




