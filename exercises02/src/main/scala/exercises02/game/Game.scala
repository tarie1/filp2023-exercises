package exercises02.game

class Game(controller: GameController) {

  /**
    * Игра угадай число
    * Ввод и вывод необходимо осуществлять с помощью методов controller
    *
    * Игра должна вызывать controller.askNumber перед каждой попыткой игрока угадать число
    * И вызвать controller.nextLine для получения ввода игрока
    * Если игрок ввел число меньше загаданного, игра должна вызвать controller.numberIsBigger
    * Если игрок ввел число больше загаданного, игра должна вызвать controller.numberIsSmaller
    * Если игрок угадал число, игра должна закончиться и вызвать controller.guessed
    * Если игрок написал GameController.IGiveUp, игра должна закончиться и вызвать controller.giveUp(number)
    * Если игрок ввел неизвестную комбинацию символов, надо вызвать contoller.wrongInput и продолжить игру
    *
    * @param number загаданное число
    */
  def play(number: Int): Unit = {
    var bool: Boolean = true
    while (bool) {
      controller.askNumber()
      val player = controller.nextLine()
      if (player.equals(GameController.IGiveUp)) {
        controller.giveUp(number)
        bool = false
      } else if (player == number.toString) {
        controller.guessed()
        bool = false
      } else {
        try {
          if (player.toInt < number) controller.numberIsBigger()
          else if (player.toInt > number) controller.numberIsSmaller()
          else controller.wrongInput()
        } catch {
          case e: NumberFormatException => controller.wrongInput()
        }
      }
    }
  }
}
