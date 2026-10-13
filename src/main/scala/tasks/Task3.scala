package tasks

@main def Task3(): Unit =
  // 3a
  val positiveFunctionLiteral: Int => String = _ match
    case 0 => "zero"
    case n if n > 0 => "positive"
    case _ => "negative"

  def positiveMethodSyntax(n: Int): String = n match
    case 0 => "zero"
    case _ if n > 0 => "positive"
    case _ => "negative"

  // 3b
  val negFunctionLiteral: (String => Boolean) => (String => Boolean) = f => (s => !f(s))

  def negMethodSyntax(f: String => Boolean): String => Boolean = s => !f(s)

  val empty: String => Boolean = _ == "" // predicate on strings
  val notEmpty = negFunctionLiteral(empty)

  // 3c
  def negGenericPredicates[X](f: X => Boolean): X => Boolean = x => !f(x)


