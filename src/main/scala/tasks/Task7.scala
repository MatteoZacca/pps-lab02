package tasks

import scala.annotation.tailrec

// recursive function
def power(base: Double, exponent: Int): Double = exponent match
  case 0          => 1.0
  case n if n > 0 => base * power(base, n - 1)
  case _          => throw new IllegalArgumentException("Exponent must be non-negative")

// tail recursion
def powerTailRecursion(base: Double, exponent: Int): Double =
  require(exponent >= 0, "Exponent must be noon-negative")

  @tailrec
  def loop(e: Int, acc: Double): Double = e match
    case 0 => acc
    case n => loop(n - 1, acc * base)
  end loop

  loop(exponent, 1.0)

@main def Task7(): Unit =
  println(s"(${power(2, 3)}, ${power(5, 2)})")
  println(s"(${powerTailRecursion(2, 3)}, ${powerTailRecursion(5, 2)})")

