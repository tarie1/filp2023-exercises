package exercises03

import scala.::
import scala.annotation.tailrec

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {

  def sum(list: MyList[Int]): Int = {
    @tailrec
    def sum_loop(result: MyList[Int], remaining: Int): Int = {
      result match {
        case Nil              => remaining
        case Cons(head, tail) => sum_loop(tail, remaining + head)
      }
    }
    sum_loop(list, 0)
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
