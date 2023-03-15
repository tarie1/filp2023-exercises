package exercises03

object Partial {

  def combo[I, T](funcs: List[PartialFunction[I, T]]): I => Option[T] = {
    funcs.foldLeft[PartialFunction[I, T]](PartialFunction.empty)(_ orElse _).lift
  }
}
