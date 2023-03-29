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
  final val regex = "fF|Ff|Ee|eE|hH|Hh|Gg|gG".r
  def react(ipt: String): String = {
    ipt.foldLeft("")((value1, value2) =>
      if (regex.findAllIn(value1 + value2).nonEmpty)
        value1.slice(0, value1.length - 1)
      else value1 + value2
    ) match {
      case result if result == ipt => result
      case string                  => react(string)
    }
  }
}
