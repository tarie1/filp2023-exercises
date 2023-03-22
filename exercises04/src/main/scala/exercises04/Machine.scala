package exercises04

import scala.annotation.tailrec

case class Machine(locked: Boolean, candies: Int, coins: Int)

/**
  * Реализуйте вендинговый аппарат по торговле барбарисками. Правила работы аппарата следующие:
  * если в закрытый аппарат вставляется монета (Coin), то аппартат открывается
  * если повернуть ручку (Turn) у открытого аппарата, то выйдет барбариска, и аппарат закроется
  * если в аппарате кончились барбариски, то он никак не реагирует. в этом случае надо вернуть список оставшихся Inputs и закончить
  * другие действия приводят к пропуску Input
  * если Input кончился, то заканчиваем
  * Подразумевается, что вы будете использовать паттерн-матчинг и рекурсию, так как while var isInstanceOf запрещены.
  */
object Machine {
  sealed trait Input

  object Input {
    case object Coin extends Input

    case object Turn extends Input
  }
  @tailrec
  def run(machine: Machine, inputs: List[Input]): (Machine, List[Input]) =
    (machine, inputs) match {
      case (Machine(true, candies, coins), Input.Coin :: inputs) =>
        run(Machine(false, candies, coins + 1), inputs)
      case (Machine(false, candies, coins), Input.Turn :: inputs) =>
        run(Machine(true, candies - 1, coins), inputs)
      case (Machine(false, 0, coins), Input.Turn :: inputs) => (machine, Input.Turn :: inputs)
      case (Machine(false, 0, coins), Input.Coin :: inputs) => (machine, Input.Coin :: inputs)
      case (Machine(true, 0, coins), Input.Coin :: inputs)  => (machine, Input.Coin :: inputs)
      case (Machine(true, 0, coins), Input.Turn :: inputs)  => (machine, Input.Turn :: inputs)
      case (machine, Nil)                                   => (machine, inputs)
      case _                                                => (machine, Nil)
    }
}
