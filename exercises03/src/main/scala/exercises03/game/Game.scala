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
      case GiveUp          => _.giveUp(number)
      case NumberIsBigger  => _.numberIsBigger()
      case NumberIsSmaller => _.numberIsSmaller()
      case Guessed         => _.guessed()
      case WrongInput      => _.wrongInput()
    }
  }

  def completed(state: State): Boolean = state == Guessed || state == GiveUp
}
