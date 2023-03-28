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

  def size[A](t: Tree[A]): Int = fold(t)(value => 1)(_ + _ + 1)

  def max(t: Tree[Int]): Int = fold(t)(value => value)(_ max _)

  def depth[A](t: Tree[A]): Int = fold(t)(value => 1)((left, right) => 1 + (left max right))

  // тут может пригодиться явное указание типа
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(value => Leaf(f(value)): Tree[B])(Branch(_, _))
}
