package u02

object Try extends App {


  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  def name(p: Person): String = p match
    case Person.Student(n, 2024) => n
    case Person.Teacher(n, _) => n

  // println(name(Person.Student("Lebowski", 2025))) // Exception in thread "main" java.lang.ExceptionInInitializerError
  println(name(Person.Student("Fred", 2024)))
  println(name(Person.Teacher("Viroli", "PPS")))
}




