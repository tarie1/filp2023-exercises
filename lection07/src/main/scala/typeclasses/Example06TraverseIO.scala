package typeclasses

import typeclasses.Example03Applicatives.IO
import typeclasses.Example05Traverse._

object Example06TraverseIO extends App {
  def loadTag(id: Int): IO[String] =
    if (id < 5) IO.pure(s"LocalTag-$id")
    else
      IO.delay {
        println(s"Loading from network for $id")

        s"ExternalTag-$id"
      }

  val tagList: IO[List[String]] = List(1, 2, 6, 8).traverse(loadTag)
  val tagOpt: IO[Option[String]] = Option(1).traverse(loadTag)
  val tagVector: IO[Vector[String]] = Vector(1, 2, 8).traverse(loadTag)

  val tagVector2: IO[Vector[String]] = Vector(1, 2, 8).map(loadTag).sequence

  println("Running tag list")
  println(tagList.unsafeRun())
}
