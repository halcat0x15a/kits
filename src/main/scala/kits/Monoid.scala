package kits

trait Monoid[A] { A =>
  def empty: A
  def append(x: A, y: A): A
  def applicative = new Applicative[({ type F[B] = A })#F] {
    def pure[B](b: B): A = A.empty
    def apply[B, C](fb: A)(f: A): A = A.append(fb, f)
  }
}

object Monoid {
  def append[A](x: A, y: A)(implicit A: Monoid[A]): A = A.append(x, y)
  @annotation.tailrec
  def multiply[A](a: A, n: Int)(implicit A: Monoid[A]): A =
    if (n <= 0) A.empty
    else if (n == 1) a
    else multiply(A.append(a, a), n - 1)
  implicit def sum[A](implicit A: Numeric[A]) = new Monoid[A] {
    def empty: A = A.zero
    def append(x: A, y: A): A = A.plus(x, y)
  }
  implicit def product[A](implicit A: Numeric[A]) = new Monoid[A] {
    def empty: A = A.one
    def append(x: A, y: A): A = A.times(x, y)
  }
  implicit val all = new Monoid[Boolean] {
    def empty: Boolean = true
    def append(x: Boolean, y: Boolean): Boolean = x && y
  }
  implicit val any = new Monoid[Boolean] {
    def empty: Boolean = false
    def append(x: Boolean, y: Boolean): Boolean = x || y
  }
  implicit val string = new Monoid[String] {
    def empty: String = ""
    def append(x: String, y: String): String = x + y
  }
  implicit val unit = new Monoid[Unit] {
    def empty: Unit = ()
    def append(x: Unit, y: Unit): Unit = ()
  }
  implicit def list[A] = new Monoid[List[A]] {
    def empty: List[A] = Nil
    def append(x: List[A], y: List[A]): List[A] = x ::: y
  }
  implicit def vector[A] = new Monoid[Vector[A]] {
    def empty: Vector[A] = Vector.empty
    def append(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
  }
  implicit def option[A](implicit A: Monoid[A]) = new Monoid[Option[A]] {
    def empty: Option[A] = None
    def append(x: Option[A], y: Option[A]) =
      (x, y) match {
        case (None, None) => None
        case (_, None) => x
        case (None, _) => y
        case (Some(a), Some(b)) => Some(A.append(a, b))
      }
  }
  implicit def first[A] = new Monoid[Option[A]] {
    def empty: Option[A] = None
    def append(x: Option[A], y: Option[A]) = x.orElse(y)
  }
  implicit def last[A] = new Monoid[Option[A]] {
    def empty: Option[A] = None
    def append(x: Option[A], y: Option[A]) = y.orElse(x)
  }
  implicit def map[K, V](implicit V: Monoid[V]) = new Monoid[Map[K, V]] {
    def empty: Map[K, V] = Map.empty
    def append(x: Map[K, V], y: Map[K, V]): Map[K, V] =
      x.foldLeft(y) {
        case (a, (k, v)) => a.updated(k, a.get(k).fold(v)(V.append(_, v)))
      }
  }
  implicit def set[A] = new Monoid[Set[A]] {
    def empty: Set[A] = Set.empty
    def append(x: Set[A], y: Set[A]): Set[A] = x | y
  }
  implicit def endo[A] = new Monoid[A => A] {
    def empty: A => A = identity
    def append(f: A => A, g: A => A): A => A = f.andThen(g)
  }
  implicit def pair[A, B](implicit A: Monoid[A], B: Monoid[B]) = new Monoid[(A, B)] {
    def empty: (A, B) = (A.empty, B.empty)
    def append(x: (A, B), y: (A, B)): (A, B) = (A.append(x._1, y._1), B.append(x._2, y._2))
  }
  implicit def triple[A, B, C](implicit A: Monoid[A], B: Monoid[B], C: Monoid[C]) = new Monoid[(A, B, C)] {
    def empty: (A, B, C) = (A.empty, B.empty, C.empty)
    def append(x: (A, B, C), y: (A, B, C)): (A, B, C) = (A.append(x._1, y._1), B.append(x._2, y._2), C.append(x._3, y._3))
  }
  implicit def ordering[A] = new Monoid[Ordering[A]] {
    def empty = new Ordering[A] {
      def compare(a: A, b: A): Int = 0
    }
    def append(x: Ordering[A], y: Ordering[A]) = new Ordering[A] {
      def compare(a: A, b: A): Int =
        x.compare(a, b) match {
          case 0 => y.compare(a, b)
          case n => n
        }
    }
  }
}
