package typeclasses

import typeclasses.Example02Functors.Functor
import typeclasses.Example02Functors.FunctorOps

object Example03Applicatives extends App {
  trait Applicative[F[_]] extends Functor[F] {
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

    def pure[A](x: A): F[A]

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      ap(map(fa)(a => (b: B) => (a, b)))(fb)
  }

  object Applicative {
    @inline
    def apply[F[_]](implicit ev: Applicative[F]): Applicative[F] =
      ev
  }

  implicit class ApplicativeOps[F[_], A](private val fa: F[A]) {
    def product[B](fb: F[B])(implicit ap: Applicative[F]): F[(A, B)] =
      ap.product(fa, fb)
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

  println(Option("aaa").product(Option("bbb")).map { case (left, right) => left + right }) // Some(aaabbb)
  println(Option("aaa").product(None))                 // None
  println(Option.empty[String].product(Option("bbb"))) // None

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      ff.zip(fa).map { case (func, value) => func(value) }

    def pure[A](x: A): List[A] =
      List(x)

    def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  }

  implicit class PureOps[A](private val a: A) extends AnyVal {
    def pure[F[_]: Applicative]: F[A] =
      Applicative[F].pure(a)
  }

  sealed trait IO[+A] {
    def unsafeRun(): A
  }

  object IO {
    case class Pure[+A](value: A) extends IO[A] {
      def unsafeRun(): A = value
    }

    case class Lazy[+A](thunk: () => A) extends IO[A] {
      def unsafeRun(): A = thunk()
    }

    def pure[A](value: A): IO[A] =
      Pure(value)

    def delay[A](value: => A): IO[A] =
      Lazy(() => value)

    implicit val ioApplicative: Applicative[IO] = new Applicative[IO] {
      def map[A, B](fa: IO[A])(f: A => B): IO[B] =
        fa match {
          case IO.Pure(value) => IO.Pure(f(value))
          case IO.Lazy(thunk) => IO.Lazy(() => f(thunk()))
        }

      def ap[A, B](ff: IO[A => B])(fa: IO[A]): IO[B] =
        (ff, fa) match {
          case (IO.Pure(func), IO.Pure(value))           => IO.Pure(func(value))
          case (IO.Pure(func), IO.Lazy(thunk))           => IO.Lazy(() => func(thunk()))
          case (IO.Lazy(thunk), IO.Pure(value))          => IO.Lazy(() => thunk()(value))
          case (IO.Lazy(leftThunk), IO.Lazy(rightThunk)) => IO.Lazy(() => leftThunk()(rightThunk()))
        }

      def pure[A](x: A): IO[A] =
        IO.Pure(x)
    }
  }

  val first: IO[Int] =
    IO.delay {
        println("Computing first value")
        30 + 5
      }
      .map(_ + 4)

  val second: IO[String] =
    IO.delay {
        println("Computing second value")
        "aaa" + "bbb"
      }
      .map(_ + "ddd")

  println("Combining")

  val combine: IO[String] =
    first.product(second).map {
      case (first, second) =>
        s"$second-$first"
    }

  println("Running")

  println(combine.unsafeRun())
}
