package tasks

import scala.annotation.tailrec

def reverseNumber(n: Int): Int =
  @tailrec
  def helper(remaining: Int, acc: Int): Int = remaining match
    case 0 => acc
    case _ => helper(remaining / 10, (acc * 10) + (remaining % 10))
    
  end helper
  
  helper(n, 0)

@main def Task8(): Unit =
  reverseNumber(12345)

