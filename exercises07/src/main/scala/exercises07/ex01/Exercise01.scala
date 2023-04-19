package exercises07.ex01

import exercises07.data.NonEmptyList
import exercises07.ex02.TupleSyntax.Tuple2Syntax
import exercises07.typeclasses._

object Exercise01 {
  object Syntax {
    implicit class SemigroupOps[A](x: A)(implicit semigroup: Semigroup[A]) {
      def |+|(y: A): A = semigroup.combine(x, y)
    }
    implicit class ApplicativeOps[A](x: A) {
      def pure[F[_]: Applicative]: F[A] = Applicative[F].pure(x)
    }
    implicit class ApplicativeOpsOption[A, B](x: Option[A])(implicit ev: Applicative[Option]) {
      def aproduct(other: Option[B]): Option[(A, B)] = ev.product(x, other)
    }

    implicit class FordableOps[F[_]: Foldable, A](fa: F[A]) {
      def foldLeft[B](initial: B)(f: (B, A) => B): B = {
        Foldable[F].foldLeft(fa, initial)(f)
      }
      def combineAll(implicit monoid: Monoid[A]): A = Foldable[F].foldMap(fa)(identity)
    }
  }

  object Instances {
    import Syntax._

    implicit val strMonoid: Monoid[String] = new Monoid[String] {
      def empty: String                         = ""
      def combine(x: String, y: String): String = x concat y
    }

    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      def empty: Int                   = 0
      def combine(x: Int, y: Int): Int = x + y
    }

    implicit val listInstances: Traverse[List] with Applicative[List] = new Traverse[List] with Applicative[List] {
      def pure[A](x: A): List[A] = List(x)
      def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = fa match {
        case Nil          => Applicative[G].pure(List.empty[B])
        case head :: tail => (f(head), traverse(tail)(f)).mapN(_ :: _)
      }
      def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
      def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = ff.foldLeft(List[B]()) { (acc, f) =>
        acc ::: fa.map(f)
      }
      def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }

    implicit val optionInstances: Traverse[Option] with Applicative[Option] =
      new Traverse[Option] with Applicative[Option] {
        def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = fa match {
          case None    => Applicative[G].pure(None)
          case Some(a) => Applicative[G].map(f(a))(Some(_))
        }

        def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = (fa, ff) match {
          case (Some(value), Some(func)) => Some(func(value))
          case _                         => None
        }
        def pure[A](x: A): Option[A]                       = Some(x)
        def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

        def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
          case None    => b
          case Some(a) => f(b, a)
        }
      }

    implicit val nelInstances: Traverse[NonEmptyList] with Applicative[NonEmptyList] =
      new Traverse[NonEmptyList] with Applicative[NonEmptyList] {

        def ap[A, B](ff: NonEmptyList[A => B])(fa: NonEmptyList[A]): NonEmptyList[B] = ff.flatMap(fa.map(_))

        def pure[A](x: A): NonEmptyList[A] = NonEmptyList(x, List().empty)

        def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] = fa.map(f)

        def foldLeft[A, B](fa: NonEmptyList[A], b: B)(f: (B, A) => B): B = fa.tail.foldLeft(f(b, fa.head))(f)

        def traverse[G[_]: Applicative, A, B](fa: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] = ???
      }

    implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
      def empty                           = Nil
      def combine(x: List[A], y: List[A]) = x ::: y
    }
  }
}
