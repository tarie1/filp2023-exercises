package exercises07.typeclasses

trait Foldable[F[_]] {
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    foldLeft(fa, B.empty)((b, a) => B.combine(b, f(a)))
  def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
}

object Foldable {
  @inline
  def apply[F[_]: Foldable](implicit inst: Foldable[F]): Foldable[F] =
    inst
}
