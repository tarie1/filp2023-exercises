package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию сalculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def calculate(expr: Expr[T]): Result[T] =
    expr match {
      case Mul(left, right) =>
        calculate(left) match {
          case Success(val1) =>
            calculate(right) match {
              case DivisionByZero => DivisionByZero
              case Success(val2)  => Success(val1 * val2)
            }
        }
      case Plus(left, right) =>
        calculate(left) match {
          case Success(val1) =>
            calculate(right) match {
              case Success(val2)  => Success(val1 + val2)
              case DivisionByZero => DivisionByZero
            }
        }
      case Minus(left, right) =>
        calculate(left) match {
          case Success(val1) =>
            calculate(right) match {
              case DivisionByZero => DivisionByZero
              case Success(val2)  => Success(val1 - val2)
            }
        }
      case Div(left, right) =>
        calculate(right) match {
          case Success(val2) =>
            isZero(val2) match {
              case true => DivisionByZero
              case false =>
                calculate(left) match {
                  case Success(val1) => Success(val1 / val2)
                }
            }
        }
      case Val(v) => Success(v)
      case If(iff, cond, left, right) =>
        calculate(cond) match {
          case Success(value) =>
            iff(value) match {
              case true =>
                calculate(left) match {
                  case DivisionByZero => DivisionByZero
                  case Success(value) => Success(value)
                }
              case false =>
                calculate(right) match {
                  case DivisionByZero => DivisionByZero
                  case Success(value) => Success(value)
                }
            }
        }

    }
}
