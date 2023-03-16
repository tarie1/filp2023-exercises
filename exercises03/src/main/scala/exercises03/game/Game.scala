package exercises03.game

object Game {
  def parseState(input: String, number: Int): State = {
    (input.toIntOption, input) match {
      case (_, GameController.IGiveUp)     => GiveUp
      case (Some(num), _) if num < number  => NumberIsBigger
      case (Some(num), _) if num > number  => NumberIsSmaller
      case (Some(num), _) if num == number => Guessed
      case default                         => WrongInput
    }
  }
  def action(state: State, number: Int): GameController => Unit = {
    state match {
      case GiveUp          => GameController => GameController.giveUp(number)
      case NumberIsBigger  => GameController => GameController.numberIsBigger()
      case NumberIsSmaller => GameController => GameController.numberIsSmaller()
      case Guessed         => GameController => GameController.guessed()
      case WrongInput      => _.wrongInput()
    }
  }

  def completed(state: State): Boolean = state == Guessed || state == GiveUp
}
