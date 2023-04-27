package exercises07.ex01

import exercises07.data.NonEmptyList
import exercises07.typeclasses._

object Exercise01 {
  object Syntax {
    implicit class SemigroupOps[A](val a: A) extends AnyVal {
      def |+|(b: A)(implicit M: Semigroup[A]): A = M.combine(a, b)
    }

    implicit class ApplicativeOps[A](val a: A) extends AnyVal {
      def pure[F[_]](implicit A: Applicative[F]): F[A] = A.pure(a)
    }

    implicit class ApplicativeOps2[F[_], A](private val fa: F[A]) extends AnyVal {
      def ap[B](ff: F[A => B])(implicit app: Applicative[F]): F[B] = app.ap(ff)(fa)

      def product[B](fb: F[B])(implicit app: Applicative[F]): F[(A, B)] = app.product(fa, fb)

      def aproduct[B](fb: F[B])(implicit apl: Applicative[F]): F[(A, B)] = {
        apl.product(fa, fb)
      }
    }

    implicit class FoldableOps[F[_], A](val fa: F[A]) extends AnyVal {
      def combineAll(implicit F: Foldable[F], M: Monoid[A]): A = F.foldLeft(fa, M.empty)(M.combine)

      def foldLeft[B](b: B)(f: (B, A) => B)(implicit F: Foldable[F]): B = F.foldLeft(fa, b)(f)
    }

    implicit class ApplicativeFunctorOps[F[_], A](val fa: F[A]) {
      def map[B](f: A => B)(implicit A: Applicative[F]): F[B] = A.map(fa)(f)
    }

    implicit class TraverseOps[F[_], A](val fa: F[A]) extends AnyVal {
      def traverse[G[_], B](f: A => G[B])(implicit T: Traverse[F], A: Applicative[G]): G[F[B]] = T.traverse(fa)(f)
    }
  }

  object Instances {
    import Syntax._

    implicit val strMonoid = new Monoid[String] {
      def empty: String = ""

      def combine(x: String, y: String): String = x + y
    }

    implicit val intMonoid = new Monoid[Int] {
      def empty: Int = 0

      def combine(x: Int, y: Int): Int = x + y
    }

    implicit val listInstances: Traverse[List] with Applicative[List] = new Traverse[List] with Applicative[List] {
      override def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
        fa.foldLeft(List.empty[B].pure[G])((accF, next) =>
          accF.product(f(next)).map { case (acc, next) => acc.appended(next) }
        )

      def pure[A](a: A): List[A] = List(a)

      override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
        ff.zip(fa).map { case (func, value) => func(value) }

      override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }

    implicit val optionInstances: Traverse[Option] with Applicative[Option] =
      new Traverse[Option] with Applicative[Option] {
        def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit A: Applicative[G]): G[Option[B]] =
          fa.fold(A.pure(Option.empty[B]))(a => A.map(f(a))(Option(_)))

        def pure[A](a: A): Option[A] = Option(a)

        def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = ff.flatMap(f => fa.map(f))

        def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa.fold(b)(f(b, _))

        def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
      }

    implicit val nelInstances: Traverse[NonEmptyList] with Applicative[NonEmptyList] =
      new Traverse[NonEmptyList] with Applicative[NonEmptyList] {
        def traverse[G[_], A, B](fa: NonEmptyList[A])(f: A => G[B])(implicit A: Applicative[G]): G[NonEmptyList[B]] = {
          val gb = f(fa.head)
          val gbTail = fa.tail.foldRight(A.pure(List.empty[B])) { (a, acc) =>
            A.ap(A.map(f(a))(b => (xs: List[B]) => b :: xs))(acc)
          }
          A.ap(A.map(gb)(b => (xs: List[B]) => NonEmptyList(b, xs)))(gbTail)
        }

        def pure[A](a: A): NonEmptyList[A] = NonEmptyList(a, List.empty)

        def ap[A, B](ff: NonEmptyList[A => B])(fa: NonEmptyList[A]): NonEmptyList[B] =
          NonEmptyList(ff.head(fa.head), ff.tail.zip(fa.tail).map { case (f, a) => f(a) })

        def foldLeft[A, B](fa: NonEmptyList[A], b: B)(f: (B, A) => B): B = fa.tail.foldLeft(f(b, fa.head))(f)

        def map[A, B](fa: NonEmptyList[A])(f: A => B): NonEmptyList[B] =
          NonEmptyList(f(fa.head), fa.tail.map(f))
      }

    implicit def listMonoid[A] = new Monoid[List[A]] {
      def empty: List[A] = List.empty[A]

      def combine(x: List[A], y: List[A]): List[A] = x ++ y
    }
  }
}
