package exercises05.either

object EitherCombinators {

  sealed trait Either[+A, +B] {
    def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] =
      (this, other) match {
        case (Right(_), _)      => this
        case (Left(_), Left(_)) => this
        case _                  => other
      }
    def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] =
      for {
        value  <- this
        either <- other
      } yield f(value, either)
    def map[E](f: B => E): Either[A, E] = this match {
      case Left(value) => Left(value)
      case Right(val1) => Right(f(val1))
    }
    def flatMap[AA >: A, E](f: B => Either[AA, E]): Either[AA, E] = this match {
      case Right(value) => f(value)
      case Left(value)  => Left(value)
    }
  }
  case class Left[+A, +B](get: A) extends Either[A, B]

  case class Right[+A, +B](get: B) extends Either[A, B]

  object Either {
    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] = option match {
      case Some(value) => Right(value)
      case None        => Left(a)
    }
    def toOption[A](either: Either[_, A]): Option[A] = either match {
      case Right(value) => Some(value)
      case _            => None
    }
    def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      list.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] =
      traverse(list)(identity)
  }
}
