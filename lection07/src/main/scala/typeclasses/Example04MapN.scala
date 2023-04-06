package typeclasses

import Example02Functors.FunctorOps
import Example03Applicatives._

object Example04MapN extends App {
  implicit class Tuple2Syntax[F[_], A, B](private val tuple: (F[A], F[B])) extends AnyVal {
    def mapN[Z](f: (A, B) => Z)(implicit applicative: Applicative[F]): F[Z] =
      applicative.map(applicative.product(tuple._1, tuple._2)) { case (a, b) => f(a, b) }
  }

  implicit class Tuple3Syntax[F[_], A, B, C](private val tuple: (F[A], F[B], F[C])) extends AnyVal {
    def mapN[Z](f: (A, B, C) => Z)(implicit applicative: Applicative[F]): F[Z] =
      applicative.map(applicative.product(applicative.product(tuple._1, tuple._2), tuple._3)) {
        case ((a, b), c) => f(a, b, c)
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

  val third: IO[String] =
    IO.delay {
        println("Computing third value")
        "eee" + "ccc"
      }
      .map(_ + "fff")

  println("Combining")

  val combine: IO[String] =
    (first, second, third).mapN(_ + _ + _)

  println("Running")

  println(combine.unsafeRun())
}
