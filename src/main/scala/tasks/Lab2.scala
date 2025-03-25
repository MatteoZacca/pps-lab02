package tasks

object Lab2 extends App {

  // ------------------------------ TASK_3 ------------------------------

  // TASK_3a //
  // val assigned to function literal
  val positive1: Double => String = _ match
    case x if x >= 0 => "positive"
    case _ => "negative"

  // method syntax
  def positive2(x: Double): String = x match
    case x if x >= 0 => "positive"
    case _ => "negative"

  // TASK_3b //
  // val lambda
  val neg1: (String => Boolean) => String => Boolean =
    predicate => s => !predicate(s)
  // Associatività destra: predicate => (s => !predicate(s))

  val empty1: String => Boolean = _ == ""
  val notEmpty1 = neg1(empty1)

  // method syntax
  def neg2(predicate: String => Boolean): String => Boolean =
    (s: String) => !predicate(s)

  val empty2: String => Boolean = _ == ""
  val notEmpty2 = neg2(empty2)

  // TASK_3c //
  def neg3[X](predicate: X => Boolean): X => Boolean =
    x => !predicate(x) // restituisce una funzione che inverte il risultato del predicato

  //Test 1: String => Boolean
  val empty3: String => Boolean = _ == ""
  val notEmpty3 = neg3(empty3)

  //Test 2: Int => Boolean
  val positive: Int => Boolean = _ > 0
  val notPositive = neg3(positive)

  // Test 3: Boolean => Boolean
  val isTrue: Boolean => Boolean = identity
  val isFalse = neg3(isTrue)

  // ------------------------------ TASK_4 ------------------------------
  // curried and val
  val p1: Double => Double => Double => Boolean =
    x => y => z => x <= y && y == z

  // curried and def
  def p2(x: Double)(y: Double)(z: Double): Boolean =
    (x <= y) && (y == z)

  // not curried and val
  val p3: (Double, Double, Double) => Boolean =
    (x, y, z) => (x <= y) && (y == z)

  // not-curried and def
  def p4(x: Double, y: Double, z: Double): Boolean =
    (x <= y) && (y == z)

  // ------------------------------ TASK_5 ------------------------------
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    x => f(g(x))

  // ------------------------------ TASK_6 ------------------------------
  def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D =
    x => f(g(h(x)))

  // Reusing generic compose function
  def composeThree2[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D =
    x => compose(f, compose(g, h))(x)

  // ------------------------------ TASK_7 ------------------------------ 
  // recursive function without tail recursion
  def power(base: Double, exp: Int): Double = exp match
    case 0 => 1
    case _ if exp > 0 => base * power(base, exp - 1)
  // println(s"Task_7[power_without_tail_recursion]: ${power(4.0, -2)}")

  // recursive function with tail recursion
  def powerTail(base: Double, exp: Int): Double =
    @annotation.tailrec
    def _pow(base: Double, exp: Int, acc: Double): Double = exp match
      case 0 => acc
      case _ if exp > 0 => _pow(base, exp - 1, base * acc)

    _pow(base, exp, 1)
  // println(s"Task_7[power_with_tail_recursion]: ${powerTail(4.0, -2)}")

  // ------------------------------ TASK_8 ------------------------------
  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def helper(remaining: Int, reversed: Int): Int =
      if (remaining == 0) reversed // no more digit to process
      else
        val lastDigit = remaining % 10
        helper(remaining / 10, reversed * 10 + lastDigit)

    helper(n, 0)

  // ------------------------------ TASK_9 ------------------------------
  enum Expr:
    case Literal(constant: Int)
    case Add(subExpr1: Expr, subExpr2: Expr)
    case Multiply(subExpr1: Expr, subExpr2: Expr)

  object Expr:
    def evaluate(expr: Expr): Int = expr match
      case Literal(constant) => constant
      case Add(subExpr1, subExpr2) => evaluate(subExpr1) + evaluate(subExpr2)
      case Multiply(subExpr1, subExpr2) => evaluate(subExpr1) * evaluate(subExpr2)

    def show(expr: Expr): String = expr match
      case Literal(constant) => constant.toString
      case Add(subExpr1, subExpr2) => s"(${show(subExpr1)} + ${show(subExpr2)})" // "(" + show(subExpr1) + "+" + show(subExpr2) + ")"
      case Multiply(subExpr1, subExpr2)  => s"(${show(subExpr1)} * ${show(subExpr2)})" // "(" + show(subExpr1) + "*" + show(subExpr2) + ")"

  // ------------------------------ TASK_10 ------------------------------
  enum Optional[A]:
    case Maybe(value: A)
    case Empty()
  
  import Optional.*

  def map[A, B](optional: Optional[A], f: A => B): Optional[B] = optional match
    case Maybe(value) => Maybe(f(value))
    case Empty() => Empty()

  def filter[A](optional: Optional[A], f: A => Boolean): Optional[A] = optional match
    case Maybe(value) if f(value) => Maybe(value)
    case _ => Empty()
}
