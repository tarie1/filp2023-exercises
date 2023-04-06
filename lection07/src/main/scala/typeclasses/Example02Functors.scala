package typeclasses

object Example02Functors extends App {
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    @inline
    def apply[F[_]](implicit ev: Functor[F]): Functor[F] =
      ev
  }

  implicit class FunctorOps[F[_], A](private val fa: F[A]) extends AnyVal {
    def map[B](f: A => B)(implicit functor: Functor[F]): F[B] =
      functor.map(fa)(f)
  }

  def plus1[F[_]: Functor](f: F[Int]): F[Int] =
    f.map(_ + 1)

  // Identity
  type Id[A] = A

  implicit val idFunctor: Functor[Id] = new Functor[Id] {
    def map[A, B](fa: A)(f: A => B): B =
      f(fa)
  }

  println(plus1[Id](10)) // 11

  // Option
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa match {
        case Some(value) => Some(f(value))
        case None        => None
      }
  }

  println(plus1[Option](Some(10))) // Some(11)
  println(plus1[Option](None)) // None

  // List

  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
  }

  println(plus1(List(1, 2))) // List(2, 3)

  // Function
  implicit def functionFunctor[From]: Functor[Function1[From, *]] = new Functor[Function1[From, *]] {
    def map[A, B](fa: From => A)(f: A => B): From => B =
      fa.andThen(f)
  }

  println(plus1[Function[Int, *]](_ + 3).apply(0)) // 4

  // IO

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
  }

  implicit val ioFunctor: Functor[IO] = new Functor[IO] {
    def map[A, B](fa: IO[A])(f: A => B): IO[B] =
      fa match {
        case IO.Pure(value) => IO.Pure(f(value))
        case IO.Lazy(thunk) => IO.Lazy(() => f(thunk()))
      }
  }

  println(IO.pure(40).map(_ + 10).unsafeRun()) // 50

  val compute: IO[Int] =
    IO.delay {
        println("Computing")
        30
      }
      .map(_ + 10)
      .map(_ + 20)

  println("Starting compute")

  println(compute.unsafeRun())
}
