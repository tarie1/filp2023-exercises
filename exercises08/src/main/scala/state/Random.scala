package state

import state.StatelessRandom.rnd
import typeclasses.Monad

object StatefulRandom {

  import scala.util.Random

  println(Random.nextDouble)
  println(Random.nextInt)
  println(Random.nextInt)

}

object StatelessRandom {

  trait Random {
    def nextInt: (Int, Random)
  }

  case class SimpleRandom(seed: Long) extends Random {
    override def nextInt: (Int, Random) = {
      val newSeed = 13 * seed + 41
      val int     = (newSeed >>> 3).toInt
      (int, SimpleRandom(newSeed))
    }
  }

  val rnd: SimpleRandom     = SimpleRandom(12)
  val (firstInt, nextRnd1)  = rnd.nextInt
  val (secondInt, nextRnd2) = nextRnd1.nextInt
  val (thirdInt, nextRnd3)  = nextRnd2.nextInt

  def pair(rnd: Random): ((Int, Int), Random) = {
    val (x, random1) = rnd.nextInt
    val (y, random2) = random1.nextInt
    ((x, y), random2)
  }

  def nonNegativeInt(rnd: Random): (Int, Random) = {
    val (x, random) = rnd.nextInt
    (if (x < 0) -x - 1 else x, random)
  }
  def double(rnd: Random): (Double, Random) = {
    val (x, random) = nonNegativeInt(rnd)
    (x / Int.MaxValue.toDouble, random)
  }
}
object BetterStatelessRandom {

  import typeclasses.Monad.syntax._
  import StatelessRandom.Random
  case class RandomState[A](run: Random => (A, Random))

  object RandomState {
    implicit val monad: Monad[RandomState] = new Monad[RandomState] {
      def pure[A](a: A): RandomState[A] = RandomState(x => (a, x))

      def map[A, B](fa: RandomState[A])(f: A => B): RandomState[B] =
        RandomState[B](x => {
          val (value, random) = fa.run(x)
          (f(value), random)
        })

      def flatMap[A, B](fa: RandomState[A])(f: A => RandomState[B]): RandomState[B] =
        RandomState[B](x =>
          fa.run(x) match {
            case (value, random) => f(value).run(random)
          }
        )
    }
  }
  val nextInt: RandomState[Int] = RandomState(x => x.nextInt)

  val nonNegativeInt: RandomState[Int] = nextInt.map(_.abs)

  val pair: RandomState[(Int, Int)] = for {
    x <- nextInt
    y <- nextInt
  } yield (x, y)

  val double: RandomState[Double] = for {
    x <- nonNegativeInt
  } yield x / (Int.MaxValue.toDouble + 1)

  val randomList: RandomState[List[Int]] = for {
    len  <- nonNegativeInt.map(_ % 10)
    list <- sequence(List.fill(len)(nextInt))
  } yield list

  def sequence[A](xs: List[RandomState[A]]): RandomState[List[A]] =
    xs.foldLeft(List.empty[A].pure[RandomState])((acc, rnd) =>
      RandomState(x => {
        val (list, random)  = acc.run(x)
        val (value, result) = rnd.run(random)
        (value :: list, result)
      })
    )
}
