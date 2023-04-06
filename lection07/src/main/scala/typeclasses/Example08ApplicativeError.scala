package typeclasses

import typeclasses.Example03Applicatives.Applicative

object Example08ApplicativeError extends App {
  trait ApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](e: E): F[A]

    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  object ApplicativeError {
    @inline
    def apply[F[_], E](implicit inst: ApplicativeError[F, E]): ApplicativeError[F, E] =
      inst
  }

  implicit class HandleErrorSyntax[F[_], A](private val fa: F[A]) extends AnyVal {
    def handleErrorWith[E](f: E => F[A])(implicit applicativeError: ApplicativeError[F, E]): F[A] =
      applicativeError.handleErrorWith(fa)(f)
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

    case class Error(error: Throwable) extends IO[Nothing] {
      def unsafeRun(): Nothing = throw error
    }

    def pure[A](value: A): IO[A] =
      Pure(value)

    def delay[A](value: => A): IO[A] =
      Lazy(() => value)

    def error(err: Throwable): IO[Nothing] =
      Error(err)

    implicit def ioApplicativeError: ApplicativeError[IO, Throwable] = new ApplicativeError[IO, Throwable] {
      def map[A, B](fa: IO[A])(f: A => B): IO[B] =
        fa match {
          case IO.Pure(value)  => IO.Pure(f(value))
          case IO.Lazy(thunk)  => IO.Lazy(() => f(thunk()))
          case IO.Error(error) => IO.error(error)
        }

      def ap[A, B](ff: IO[A => B])(fa: IO[A]): IO[B] =
        (ff, fa) match {
          case (IO.Pure(func), IO.Pure(value))           => IO.Pure(func(value))
          case (IO.Pure(func), IO.Lazy(thunk))           => IO.Lazy(() => func(thunk()))
          case (IO.Lazy(thunk), IO.Pure(value))          => IO.Lazy(() => thunk()(value))
          case (IO.Lazy(leftThunk), IO.Lazy(rightThunk)) => IO.Lazy(() => leftThunk()(rightThunk()))
          case (IO.Error(err), _)                        => IO.Error(err)
          case (_, IO.Error(err))                        => IO.Error(err)
        }

      def pure[A](x: A): IO[A] =
        IO.Pure(x)

      def raiseError[A](e: Throwable): IO[A] =
        IO.Error(e)

      def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]): IO[A] =
        fa match {
          case IO.Error(e) => f(e)
          case other       => other
        }
    }
  }

  def loadTag(id: Int): IO[String] =
    if (id < 5) IO.pure(s"LocalTag-$id")
    else if (id > 6)
      IO.delay {
        println(s"Loading from network for $id")

        s"ExternalTag-$id"
      } else IO.error(new RuntimeException("No such tag"))

  println(loadTag(1).unsafeRun())

  println(loadTag(7).unsafeRun())

  println(loadTag(6).handleErrorWith[Throwable](err => IO.pure(s"ErrorTag: ${err.getMessage}")).unsafeRun())
}
