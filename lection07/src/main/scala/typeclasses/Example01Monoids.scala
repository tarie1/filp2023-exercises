package typeclasses

object Example01Monoids extends App  {
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  object Semigroup {
    @inline
    def apply[A](implicit inst: Semigroup[A]): Semigroup[A] =
      inst
  }

  implicit class SemigroupOps[A](private val a: A) extends AnyVal {
    def |+|(b: A)(implicit semigroup: Semigroup[A]): A =
      semigroup.combine(a, b)
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  case class HttpRequest(method: String, url: String, body: String = "", headers: Map[String, String] = Map.empty)

  case class HttpResponse(status: Int, body: String = "", headers: Map[String, String] = Map.empty)

  type Route = PartialFunction[HttpRequest, HttpResponse]

  implicit val routeMonoid: Monoid[Route] = new Monoid[Route] {
    override val empty: Route = PartialFunction.empty

    override def combine(x: Route, y: Route): Route =
      x.orElse(y)
  }

  def mkRoute(method: String, prefix: String)(res: HttpResponse): Route = {
    case req if req.method == method && req.url.startsWith(prefix) => res
  }

  val api: Route =
    mkRoute("GET", "/cat")(HttpResponse(200, "cat")) |+|
    mkRoute("GET", "/dog")(HttpResponse(200, "dog")) |+|
    mkRoute("POST", "/upload")(HttpResponse(300, "DONED")) |+|
    PartialFunction.fromFunction(_ => HttpResponse(404, "Not Found"))


  println(api(HttpRequest("GET", "/cat/1332?a=b")))

  println(api(HttpRequest("GET", "/dog/3232")))

  println(api(HttpRequest("POST", "/upload/dog", "SOME DOG")))

  println(api(HttpRequest("GET", "/invalid")))
}
