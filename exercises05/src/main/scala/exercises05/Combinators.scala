package exercises05
object Combinators {
  // Есть цепочка hefEgGeGFEgGgeHE
  // в данной цепочке есть различные типы частиц
  // f, e, h, g положительно заряженные частицы
  // F, E, H, G отрицательно заряженные частицы
  // если частицы одного типа с разной полярностью стоят вместе в цепочке, они реагируют и исчезают
  // проход слева направо
  //
  // hefEgGeGFEgGgeHE <- gG прореагировали
  // hefEeGFEgGgeHE <- Ee прореагировали
  // hefGFEgGgeHE <- gG
  // hefGFEgeHE <- итоговая цепочка, в которой 10 частиц
  //
  // Напишите функцию, используя комбинаторы стандартной библиотеки,
  // которая проведёт полную реакцию
  def react(ipt: String): String = {
    ipt.foldLeft("") {
      case (currentString, next) =>
        currentString.takeRight(1) match {
          case element if next.isLower && next.toUpper.toString == element => currentString.dropRight(1)
          case element if next.isUpper && next.toLower.toString == element => currentString.dropRight(1)
          case _                                                           => currentString + next
        }
    }
  }
}
