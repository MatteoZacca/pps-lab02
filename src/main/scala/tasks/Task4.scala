package tasks

@main def Task4(): Unit =
  // Curried val
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z

  // Non-curried val
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z

  // Curried def
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z

  // Non-curried def
  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y == z
