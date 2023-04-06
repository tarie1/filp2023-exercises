package typeclasses

import typeclasses.Example03Applicatives.IO
import typeclasses.Example05Traverse._

object Example07SequenceIO extends App {
  def loadTag(id: Int): IO[String] =
    if (id < 5) IO.pure(s"LocalTag-$id")
    else
      IO.delay {
        println(s"Loading from network for $id")

        s"ExternalTag-$id"
      }

  val listIOTag: List[IO[String]] = List(1, 2, 6, 8).map(loadTag)

  val tagList: IO[List[String]] = listIOTag.sequence

  println("Running tag list")
  println(tagList.unsafeRun())
}
