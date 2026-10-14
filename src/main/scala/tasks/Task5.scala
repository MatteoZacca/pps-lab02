package tasks

@main def Task(): Unit =
  // functional compositions
  def compose(f: Int => Int, g: Int => Int): Int => Int = 
    x => f(g(x))

  // generic version
  def composeGeneric[A, B, C](f: B => C, g: A => B): A => C = 
    x => f(g(x))