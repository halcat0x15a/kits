package kits

trait Monoid[A] {
  def zero: A
  def append(x: A, y: A): A
}

object Monoid {
  implicit def sum[A](implicit A: Numeric[A]) = new Monoid[A] {
    def zero: A = A.zero
    def append(x: A, y: A): A = A.plus(x, y)
  }
  implicit def product[A](implicit A: Numeric[A]) = new Monoid[A] {
    def zero: A = A.one
    def append(x: A, y: A): A = A.times(x, y)
  }
  implicit val all = new Monoid[Boolean] {
    def zero: Boolean = true
    def append(x: Boolean, y: Boolean): Boolean = x && y
  }
  implicit val any = new Monoid[Boolean] {
    def zero: Boolean = false
    def append(x: Boolean, y: Boolean): Boolean = x || y
  }
  implicit val string = new Monoid[String] {
    def zero: String = ""
    def append(x: String, y: String): String = x + y
  }
  implicit val unit = new Monoid[Unit] {
    def zero: Unit = ()
    def append(x: Unit, y: Unit): Unit = ()
  }
  implicit def list[A] = new Monoid[List[A]] {
    def zero: List[A] = Nil
    def append(x: List[A], y: List[A]): List[A] = x ::: y
  }
  implicit def vector[A] = new Monoid[Vector[A]] {
    def zero: Vector[A] = Vector.empty
    def append(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
  }
  implicit def option[A](implicit A: Monoid[A]) = new Monoid[Option[A]] {
    def zero: Option[A] = None
    def append(x: Option[A], y: Option[A]) =
      (x, y) match {
        case (None, None) => None
        case (_, None) => x
        case (None, _) => y
        case (Some(a), Some(b)) => Some(A.append(a, b))
      }
  }
  implicit def first[A] = new Monoid[Option[A]] {
    def zero: Option[A] = None
    def append(x: Option[A], y: Option[A]) = x.orElse(y)
  }
  implicit def last[A] = new Monoid[Option[A]] {
    def zero: Option[A] = None
    def append(x: Option[A], y: Option[A]) = y.orElse(x)
  }
  implicit def map[K, V](implicit V: Monoid[V]) = new Monoid[Map[K, V]] {
    def zero: Map[K, V] = Map.empty
    def append(x: Map[K, V], y: Map[K, V]): Map[K, V] =
      x.foldLeft(y) {
        case (a, (k, v)) => a.updated(k, a.get(k).fold(v)(V.append(_, v)))
      }
  }
  implicit def set[A] = new Monoid[Set[A]] {
    def zero: Set[A] = Set.empty
    def append(x: Set[A], y: Set[A]): Set[A] = x | y
  }
  implicit def endo[A] = new Monoid[A => A] {
    def zero: A => A = identity
    def append(f: A => A, g: A => A): A => A = f.andThen(g)
  }
}
