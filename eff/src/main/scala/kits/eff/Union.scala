package kits.eff

sealed abstract class Union[-A] extends Product with Serializable

object Union {
  def apply[A](a: A)(implicit A: Manifest[A]): Union[~[A]] = Tagged(A, a)

  case class Tagless[A](value: Any) extends Union[A]

  case class Tagged[A](tag: Manifest[_], value: Any) extends Union[A]
}
