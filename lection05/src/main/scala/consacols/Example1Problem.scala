package consacols

object Example1Problem extends App {

  trait Storage {
    def getById(id: Int): String
  }

  object StorageImplementation extends Storage {
    def getById(id: Int): String = id match {
      case 1 => "kek"
      case 2 => "cheburek"
      case _ => null
    }
  }


  println("Хелло, я запустился")
  println(StorageImplementation.getById(42).map(_.toUpper))
  println("О нет, пользователь меня никогда не увидит :'(")

}
