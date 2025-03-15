package tasks

object Lab2 extends App {

  // ------------------------------ TASK_3 ------------------------------

  // TASK_3a //
  // val assigned to function literal
  val positive1: Double => String = _ match
    case x if x >= 0 => "positive"
    case _ => "negative"
  
  println(s"Task_3a[val assigned to function literal]: ${positive1(80)}")
  println(s"Task_3a[val assigned to function literal]: ${positive1(0)}")
  println(s"Task_3a[val assigned to function literal]: ${positive1(-80)}")


  // method syntax
  def positive2(x: Double): String = x match
    case x if x >= 0 => "positive"
    case _ => "negative"

  println(s"Task_3a[method syntax]: ${positive2(80)}")
  println(s"Task_3a[method syntax]: ${positive2(0)}")
  println(s"Task_3a[method syntax]: ${positive2(-80)}")


  // TASK_3b //
  // val lambda
  val neg1: (String => Boolean) => String => Boolean =
    predicate => s => !predicate(s)
    // Associatività destra: predicate => (s => !predicate(s))

  val empty1: String => Boolean = _ == ""
  val notEmpty1 = neg1(empty1)

  println(notEmpty1(""))
  println(notEmpty1("Scala"))

  // method syntax
  def neg2(predicate: String => Boolean): String => Boolean =
    (s: String) => !predicate(s)

  val empty2: String => Boolean = _ == ""
  val notEmpty2 = neg2(empty2)

  println(notEmpty2(""))
  println(notEmpty2("Scala"))


  // TASK_3c //
  def neg3[X](predicate: X => Boolean): X => Boolean =
    x => !predicate(x) // restituisce una funzione che inverte il risultato del predicato

  //Test 1: String => Boolean
  val empty3: String => Boolean = _ == ""
  val notEmpty3 = neg3(empty3)

  println(notEmpty3(""))
  println(notEmpty3("Scala"))

  //Test 2: Int => Boolean
  val positive: Int => Boolean = _ > 0
  val notPositive = neg3(positive)

  println(notPositive(10))
  println(notPositive(-5))
  println(notPositive(0))

  // Test 3: Boolean => Boolean
  val isTrue: Boolean => Boolean = identity
  val isFalse = neg3(isTrue)

  println(isFalse(true))
  println(isFalse(false))
  
  
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
  
  println(s"Task_4[curried/val]: ${p1(3.2)(6.4)(6.4)}")
  println(s"Task_4[curried/val]: ${p1(6.5)(6.4)(6.4)}")
  println(s"Task_4[curried/def]: ${p2(3.2)(6.4)(6.4)}")
  println(s"Task_4[curried/def]: ${p2(6.5)(6.4)(6.4)}")
  println(s"Task_4[not curried/val]: ${p3(3.2, 6.4, 6.4)}")
  println(s"Task_4[not curried/val]: ${p3(6.5, 6.4, 6.4)}")
  println(s"Task_4[not curried/def]: ${p4(3.2, 6.4, 6.4)}")
  println(s"Task_4[not curried/def]: ${p4(6.5, 6.4, 6.4)}")
  
  // ------------------------------ TASK_5 ------------------------------
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    x => f(g(x))
    
  println(s"Task_5[generic compose]: ${compose((x: Int) => x - 1, (x: Int) => x * 2)(5)}")  

  // ------------------------------ TASK_6 ------------------------------
  def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D =
    x => f(g(h(x)))
    
  println(s"Task_6[composeThree]: ${composeThree((x: Int) => x - 1, (x: Int) => x - 2, (x: Int) => x - 3)(10)}")  
  
  def composeThree2[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D = 
    x => compose(f, compose(g, h))(x)
  
  println(s"Task_6[composeThree2]: ${composeThree2((x: Int) => x - 1, (x: Int) => x - 2, (x: Int) => x - 3)(10)}")
  
}
