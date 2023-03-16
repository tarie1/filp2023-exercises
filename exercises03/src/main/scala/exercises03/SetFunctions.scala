package exercises03

object SetFunctions {
  type Set[A] = A => Boolean

  def contains[A](s: Set[A], elem: A): Boolean = s(elem)

  def singletonSet[A](elem: A): Set[A] = element => elem == element

  def union[A](s: Set[A], t: Set[A]): Set[A] = element => s(element) || t(element)

  def intersect[A](s: Set[A], t: Set[A]): Set[A] = element => s(element) && t(element)

  def diff[A](s: Set[A], t: Set[A]): Set[A] = element => s(element) && !t(element)

  def symmetricDiff[A](s: Set[A], t: Set[A]): Set[A] =
    element => (s(element) && !t(element)) || (!s(element) && t(element))

  def filter[A](s: Set[A], p: A => Boolean): Set[A] = element => p(element)

  def cartesianProduct[A, B](as: Set[A], bs: Set[B]): Set[(A, B)] = { case (a, b) => as(a) && bs(b) }
}
