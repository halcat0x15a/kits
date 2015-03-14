package kits

trait Plus[F[_]] {
  def empty[A]: F[A]
  def plus[A](x: F[A], y: F[A]): F[A]
}

object Plus {
  implicit val list = new Plus[List] {
    def empty[A]: List[A] = Nil
    def plus[A](x: List[A], y: List[A]): List[A] = x ::: y
  }
  implicit val vector = new Plus[Vector] {
    def empty[A]: Vector[A] = Vector.empty[A]
    def plus[A](x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
  }
  implicit val option = new Plus[Option] {
    def empty[A]: Option[A] = None
    def plus[A](x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
  }
  implicit def map[K] = new Plus[({ type F[A] = Map[K, A] })#F] {
    def empty[A]: Map[K, A] = Map.empty[K, A]
    def plus[A](x: Map[K, A], y: Map[K, A]): Map[K, A] = x ++ y
  }
  implicit val set = new Plus[Set] {
    def empty[A]: Set[A] = Set.empty[A]
    def plus[A](x: Set[A], y: Set[A]): Set[A] = x | y
  }
}
