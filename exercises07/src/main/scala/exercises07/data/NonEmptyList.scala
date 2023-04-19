package exercises07.data

import exercises07.typeclasses.Semigroup

case class NonEmptyList[+A](head: A, tail: List[A] = Nil) {
  def toList: List[A]                    = head :: tail
  def map[B](f: A => B): NonEmptyList[B] = NonEmptyList(f(head), tail.map(f))

  def flatMap[B](fa: A => NonEmptyList[B]): NonEmptyList[B] =
    NonEmptyList(fa(head).head, fa(head).tail ::: tail.flatMap(value => fa(value).toList))
}

object NonEmptyList {
  def of[A](head: A, tail: A*): NonEmptyList[A] = NonEmptyList(head, tail.toList)

  implicit def nelSemigroup[A]: Semigroup[NonEmptyList[A]] =
    (x, y) => NonEmptyList(x.head, x.tail ::: (y.head :: y.tail))
}
