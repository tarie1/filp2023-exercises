package exercises03

import scala.::
import scala.annotation.tailrec

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {

  def sum(list: MyList[Int]): Int = list match {
    case Nil              => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def reverse[A](list: MyList[A]): MyList[A] = {
    @tailrec
    def reverse_loop[A](result: MyList[A], remaining: MyList[A]): MyList[A] = {
      result match {
        case Nil              => remaining
        case Cons(head, tail) => reverse_loop(tail, Cons(head, remaining))
      }
    }
    reverse_loop(list, Nil)
  }
}
