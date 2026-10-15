package tasks

object Task10:

  enum Optional[A]:
    case Maybe(value: A)
    case Empty()

  object Optional:

    def isEmpty[A](opt: Optional[A]): Boolean = opt match
      case Empty() => true
      case _       => false

    def orElse[A](opt: Optional[A], default: A): A = opt match
      case Maybe(a) => a
      case _        => default

    // map transforms the inner value if it exists
    def map[A, B](opt: Optional[A])(f: A => B): Optional[B] = opt match
      case Maybe(a) => Maybe(f(a))
      case Empty()  => Empty()

    // filter keeps the value only if the predicate returns true
    def filter[A](opt: Optional[A])(predicate: A => Boolean): Optional[A] = opt match
      case Maybe(a) if predicate(a) => Maybe(a)
      case Maybe(_)                 => Empty()
      case Empty()                  => Empty()

