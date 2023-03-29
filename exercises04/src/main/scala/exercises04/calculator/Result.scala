package exercises04.calculator

sealed trait Result[+A] {
  def map[B](f: A => B): Result[B] = this match {
    case Success(a)     => Success(f(a))
    case DivisionByZero => DivisionByZero
  }
  def flatMap[B](f: A => Result[B]): Result[B] = this match {
    case DivisionByZero => DivisionByZero
    case Success(value) => f(value)
  }
}
case class Success[+A](value: A) extends Result[A] {}
case object DivisionByZero       extends Result[Nothing]
