package writer.app

import typeclasses.{Monoid, Semigroup}

object domain {
  case class Good(price: BigDecimal)
  case class Wallet(amount: BigDecimal)
  case class Transaction(price: BigDecimal)
  object Transaction {
    implicit val semigroup: Semigroup[Transaction] = (x, y) => Transaction(x.price + y.price)

    implicit val monoid = new Monoid[Transaction] {
      override def empty: Transaction = Transaction(BigDecimal(0))

      override def combine(x: Transaction, y: Transaction): Transaction = semigroup.combine(x, y)
    }
  }
}
