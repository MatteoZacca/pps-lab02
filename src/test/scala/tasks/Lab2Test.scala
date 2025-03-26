package tasks

import org.junit.*
import org.junit.Assert.*

class Lab2Test:
  import Lab2.*
  import Lab2.Expr

  @Test def task_3a(): Unit =
    // val assigned to function literal
    assertEquals("positive", positive1(80))
    assertEquals("positive", positive1(0))
    assertEquals("negative", positive1(-80))
    // method syntax
    assertEquals("positive", positive2(80))
    assertEquals("positive", positive2(0))
    assertEquals("negative", positive2(-80))

  @Test def task_3b(): Unit =
    // val lambda
    assertEquals(false, notEmpty1(""))
    assertEquals(true, notEmpty1("Scala"))
    // method syntax
    assertEquals(false, notEmpty2(""))
    assertEquals(true, notEmpty2("Scala"))

  @Test def task_3c(): Unit =
    // Test 1: String => Boolean
    assertEquals(false, notEmpty3(""))
    assertEquals(true, notEmpty3("Scala"))
    // Test 2: Int => Boolean
    assertEquals(false, notPositive(10))
    assertEquals(true, notPositive(-5))
    assertEquals(true, notPositive(0))
    // Test 3: Boolean => Boolean
    assertEquals(false, isFalse(true))
    assertEquals(true, isFalse(false))

  @Test def task_4(): Unit =
    // curried - val
    assertEquals(true, p1(3.2)(6.4)(6.4))
    assertEquals(false, p1(6.5)(6.4)(6.4))
    // curried - def
    assertEquals(true, p2(3.2)(6.4)(6.4))
    assertEquals(false, p2(6.5)(6.4)(6.4))
    // not curried - val
    assertEquals(true, p3(3.2, 6.4, 6.4))
    assertEquals(false, p3(6.5, 6.4, 6.4))
    // not curried - def
    assertEquals(true, p4(3.2, 6.4, 6.4))
    assertEquals(false, p4(6.5, 6.4, 6.4))

  @Test def task_5(): Unit =
    // generic compose
    assertEquals(9, compose((x: Int) => x - 1, (x: Int) => x * 2)(5))

  @Test def task_6(): Unit =
    // composeThree
    assertEquals(4, composeThree((x: Int) => x - 1, (x: Int) => x - 2, (x: Int) => x - 3)(10))
    // composeThree2
    assertEquals(4, composeThree2((x: Int) => x - 1, (x: Int) => x - 2, (x: Int) => x - 3)(10))

  @Test def task_7(): Unit =
    // power without tail recursion
    assertEquals(16.0, power(4.0, 2), 0.000)
    assertEquals(1.0, power(4.0, 0), 0.000)
    assertEquals(4.0, power(4.0, 1), 0.000)
    // power with tail recursion
    assertEquals(16.0, powerTail(4.0, 2), 0.000)
    assertEquals(1.0, powerTail(4.0, 0), 0.000)
    assertEquals(4.0, powerTail(4.0, 1), 0.000)

  @Test def task_8(): Unit =
    // reverse Number
    assertEquals(54321, reverseNumber(12345))
    assertEquals(1, reverseNumber(100))

  @Test def task_9(): Unit =
    // simple expression
    val expr1 = Expr.Literal(7)
    assertEquals(7, Expr.evaluate(expr1))
    assertEquals("7", Expr.show(expr1))
    // addition
    val expr2 = Expr.Add(Expr.Literal(4), Expr.Literal(3))
    assertEquals(7, Expr.evaluate(expr2))
    assertEquals("(4 + 3)", Expr.show(expr2))
    // multiplication
    val expr3 = Expr.Multiply(Expr.Literal(5), Expr.Literal(6))
    assertEquals(30, Expr.evaluate(expr3))
    assertEquals("(5 * 6)", Expr.show(expr3))
    // complicated expression
    val expr4 = Expr.Multiply(Expr.Add(Expr.Literal(3), Expr.Literal(2)), Expr.Literal(4))
    assertEquals(20, Expr.evaluate(expr4))
    assertEquals("((3 + 2) * 4)", Expr.show(expr4))
    // nested expression
    val expr5 = Expr.Add(
      Expr.Multiply(Expr.Literal(2), Expr.Literal(3)),
      Expr.Add(Expr.Literal(1), Expr.Literal(4))
    )
    assertEquals(11, Expr.evaluate(expr5))
    assertEquals("((2 * 3) + (1 + 4))", Expr.show(expr5))
















