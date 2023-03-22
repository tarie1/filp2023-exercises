package variance

object CovarianceExample extends App {

  trait Fruit
  trait Apple extends Fruit
  case class Antonovka() extends Apple

  trait Show[-T] {
    def show(v: T): String
  }

  val fruitShow: Show[Fruit] = new Show[Fruit] {
    override def show(v: Fruit): String = "Фрукт"
  }

  /**
    * Еще один вариант создание инстанса.
    * Нам нужно реализовать всего один метод, поэтому скала позволяет нам использовать подобную конструкцию
    * Обязательно нужно указывать тип значения.
    * Подробнее можно ознакомиться в документации https://scala-lang.org/news/2.12.0/#lambda-syntax-for-sam-types
    */
  val anotherFruitShow: Show[Fruit] = _ => "Фрукт"

  val antonovkaShow: Show[Antonovka] = fruitShow

  val apple = Antonovka()

  println(fruitShow.show(apple))
  println(anotherFruitShow.show(apple))

  println(antonovkaShow.show(apple))

}
