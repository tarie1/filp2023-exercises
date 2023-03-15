package exercises02.game

import scala.annotation.tailrec

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
  @tailrec
  final def play(number: Int): Unit = {
    controller.askNumber()
    val player = controller.nextLine()
    (player.toIntOption, player) match {
      case (Some(num), _) if num == number => controller.guessed()
      case (_, GameController.IGiveUp)     => controller.giveUp(number)
      case (Some(num), _) if num > number => {
        controller.numberIsSmaller()
        play(number)
      }
      case (Some(num), _) if num < number => {
        controller.numberIsBigger()
        play(number)
      }
      case default => {
        controller.wrongInput()
        play(number)
      }
    }
  }
}
