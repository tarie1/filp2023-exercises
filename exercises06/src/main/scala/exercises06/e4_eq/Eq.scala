package exercises06.e4_eq

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}
object Eq {
  def apply[A](implicit eq: Eq[A]): Eq[A] = eq
}
object EqInstances {
  implicit def valEq[A]: Eq[A] = (a, b) => a == b
  implicit def listEq[A](implicit eq: Eq[A]): Eq[List[A]] =
    (a, b) =>
      (a, b) match {
        case (head1 :: tail1, head2 :: tail2) => eq.eqv(head1, head2) && listEq[A].eqv(tail1, tail2)
        case (Nil, Nil)                       => true
        case _                                => false
      }
  implicit def optionEq[A](implicit eq: Eq[A]): Eq[Option[A]] =
    (a, b) =>
      (a, b) match {
        case (Some(valueA), Some(valueB)) => eq.eqv(valueA, valueB)
        case (None, None)                 => true
        case _                            => false
      }
}

object EqSyntax {
  implicit class EqOps[A](val value: A) extends AnyVal {
    def eqv(comparable: A)(implicit eq: Eq[A]): Boolean   = eq.eqv(value, comparable)
    def ===(comparable: A)(implicit eq: Eq[A]): Boolean   = eq.eqv(value, comparable)
    def !==(incomparable: A)(implicit eq: Eq[A]): Boolean = !eq.eqv(value, incomparable)
  }
}

object Examples {
  import EqInstances._
  import EqSyntax._

  1 eqv 1 // возвращает true
  1 === 2 // возвращает false
  1 !== 2 // возвращает true
//  1 === "some-string" // не компилируется
//  1 !== Some(2) // не компилируется
  List(true) === List(true) // возвращает true
}
