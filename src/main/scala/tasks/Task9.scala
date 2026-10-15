package tasks

import tasks.Expr.Literal

enum Expr:
  case Literal(constant: Int)
  case Add(addend1: Expr, addend2: Expr)
  case Multiply(factor1: Expr, factor2: Expr)

// Companion Object: must have the same name and be in the same file
object Expr:
  def evaluate(expr: Expr): Int = expr match
    case Literal(c)       => c
    case Add(e1, e2)      => evaluate(e1) + evaluate(e2)
    case Multiply(e1, e2) => evaluate(e1) * evaluate(e2)

  def show(expr: Expr): String = expr match
    case Literal(c)  => c.toString
    case Add(e1, e2) => "(" + show(e1) + " + " + show(e2) + ")"
    case Multiply(e1, e2) => "(" + show(e1) + " * " + show(e2) + ")"

@main def Task9(): Unit =
  import Expr.*

  val myFormula = Add(Literal(5), Multiply(Literal(2), Literal(3)))

  println(show(myFormula))
  println(evaluate(myFormula))



