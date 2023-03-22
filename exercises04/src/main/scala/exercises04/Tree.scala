package exercises04

import scala.annotation.tailrec

sealed trait Tree[+A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Необходимо реализовать операции на бинарном дереве
object Tree {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(value) => f(value)
      case Branch(left, right) =>
        g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

  def size[A](t: Tree[A]): Int = {
    @tailrec
    def size_loop(tree: List[Tree[A]], num: Int): Int =
      tree match {
        case Nil                              => num
        case Leaf(value) :: remaining         => size_loop(remaining, num + 1)
        case Branch(left, right) :: remaining => size_loop(left :: right :: remaining, num + 1)
      }
    size_loop(List(t), 0)
  }

  def max(t: Tree[Int]): Int = {
    def max_loop(tree: Tree[Int], num: Int): Int =
      tree match {
        case Leaf(value)         => num max value
        case Branch(left, right) => max_loop(left, num) max max_loop(right, num)
      }
    max_loop(t, -1000)
  }

  def depth[A](t: Tree[A]): Int = {
    def depth_loop(tree: Tree[A], num: Int): Int = {
      tree match {
        case Leaf(value)         => num
        case Branch(left, right) => depth_loop(left, num + 1) max depth_loop(right, num + 1)
      }
    }
    depth_loop(t, 1)
  }

  // тут может пригодиться явное указание типа
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
}
