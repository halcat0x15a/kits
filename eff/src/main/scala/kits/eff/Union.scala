package kits.eff

case class Union[-A](tag: Manifest[_], value: Any)

object Union {
  def apply[A](a: A)(implicit A: Manifest[A]): Union[~[A]] = new Union(A, a)
}
