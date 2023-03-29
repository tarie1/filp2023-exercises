package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def calculate(expr: Expr[T]): Result[T] = {
    expr match {
      case Mul(left, right)   => calculate(left).flatMap(val1 => calculate(right).map(val2 => val1 * val2))
      case Plus(left, right)  => calculate(left).flatMap(val1 => calculate(right).map(val2 => val1 + val2))
      case Minus(left, right) => calculate(left).flatMap(val1 => calculate(right).map(val2 => val1 - val2))
      case Div(left, right) =>
        calculate(right).flatMap(val2 =>
          isZero(val2) match {
            case true  => DivisionByZero
            case false => calculate(left).map(val1 => val1 / val2)
          }
        )
      case Val(v) => Success(v)
      case If(iff, cond, left, right) =>
        calculate(cond).flatMap(value =>
          iff(value) match {
            case true  => calculate(left)
            case false => calculate(right)
          }
        )
    }
  }
}
