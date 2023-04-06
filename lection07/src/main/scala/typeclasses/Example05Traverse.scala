package typeclasses

import typeclasses.Example02Functors.FunctorOps
import typeclasses.Example03Applicatives.{Applicative, ApplicativeOps, PureOps}

object Example05Traverse extends App {
  trait Traverse[F[_]] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  }

  object Traverse {
    @inline
    def apply[F[_]](implicit ev: Traverse[F]): Traverse[F] =
      ev
  }

  implicit class TraverseOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def traverse[G[_]: Applicative, B](f: A => G[B])(implicit traverse: Traverse[F]): G[F[B]] =
      traverse.traverse(fa)(f)
  }

  implicit class SequenceOps[F[_], G[_], A](private val gfa: G[F[A]]) extends AnyVal {
    def sequence(implicit applicative: Applicative[F], traverse: Traverse[G]): F[G[A]] =
      traverse.traverse(gfa)(identity)
  }

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      (fa, ff) match {
        case (Some(value), Some(func)) => Some(func(value))
        case _                         => None
      }

    def pure[A](x: A): Option[A] =
      Some(x)

    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.map(f)
  }

  // Not optimal
  implicit lazy val listTraverse: Traverse[List] = new Traverse[List] {
    def traverse[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] =
      fa.foldLeft(List.empty[B].pure[G])((accF, next) =>
        accF.product(f(next)).map { case (acc, next) => acc.appended(next) }
      )
  }

  implicit lazy val vectorTraverse: Traverse[Vector] = new Traverse[Vector] {
    def traverse[G[_]: Applicative, A, B](fa: Vector[A])(f: A => G[B]): G[Vector[B]] =
      fa.foldLeft(Vector.empty[B].pure[G])((accF, next) =>
        accF.product(f(next)).map { case (acc, next) => acc.appended(next) }
      )
  }

  implicit lazy val optionTraverse: Traverse[Option] = new Traverse[Option] {
    def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] =
      fa match {
        case Some(value) => f(value).map(Some(_))
        case None        => Option.empty[B].pure[G]
      }
  }

  def getTag(id: Int): Option[String] =
    if (id < 5) Some("Less")
    else if (id > 7) Some("Greater")
    else None

  println(List(1, 2, 3, 4, 8).traverse(getTag)) // Some(List(Less, Less, Less, Less, Greater))

  println(Vector(1, 2, 3, 4, 8).traverse(getTag)) // Some(Vector(Less, Less, Less, Less, Greater))

  println(List(1, 6).traverse(getTag)) // None

  println(Option(1).traverse(getTag)) // Some(Some(Less))

  println(Option(8).traverse(getTag)) // Some(Some(Greater))

  println(Option.empty[Int].traverse(getTag)) // Some(None)

}
