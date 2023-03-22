package consacols

// calculator try 2/option
object Example03ProblemOpt extends App {

  trait Storage {
    def getById(id: Int): Option[String]
  }

  object StorageImplementation extends Storage {
    def getById(id: Int): Option[String] = id match {
      case 1 => Some("kek")
      case 2 => Some("cheburek")
      case _ => None
    }
  }


  println("Хелло, я запустился")
  println(StorageImplementation.getById(42))
  println(StorageImplementation.getById(42).getOrElse("Строчка заглушка").map(_.toUpper))
  println("Наконец-то, теперь меня прочитают!")

}
