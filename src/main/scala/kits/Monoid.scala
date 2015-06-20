package kits

trait Monoid[A] { A =>

  def empty: A

  def append(x: A, y: A): A

  def applicative: Applicative[({ type F[B] = A })#F] =
    new Applicative[({ type F[B] = A })#F] {
      def pure[B](b: B): A = A.empty
      def apply[B, C](fb: A)(f: A): A = A.append(f, fb)
    }

}

object Monoid {

  def append[A](xs: A*)(implicit A: Monoid[A]): A = xs.foldLeft(A.empty)(A.append)

  def multiply[A: Monoid](a: A, n: Int): A = append(Seq.fill(n)(a): _*)

  implicit def sum[A](implicit A: Numeric[A]): Monoid[A] =
    new Monoid[A] {
      def empty: A = A.zero
      def append(x: A, y: A): A = A.plus(x, y)
    }

  implicit def product[A](implicit A: Numeric[A]): Monoid[A] =
    new Monoid[A] {
      def empty: A = A.one
      def append(x: A, y: A): A = A.times(x, y)
    }

  implicit val all: Monoid[Boolean] =
    new Monoid[Boolean] {
      def empty: Boolean = true
      def append(x: Boolean, y: Boolean): Boolean = x && y
    }

  implicit val any: Monoid[Boolean] =
    new Monoid[Boolean] {
      def empty: Boolean = false
      def append(x: Boolean, y: Boolean): Boolean = x || y
    }

  implicit val string: Monoid[String] =
    new Monoid[String] {
      def empty: String = ""
      def append(x: String, y: String): String = x + y
    }

  implicit val unit: Monoid[Unit] =
    new Monoid[Unit] {
      def empty: Unit = ()
      def append(x: Unit, y: Unit): Unit = ()
    }

  implicit def list[A]: Monoid[List[A]] =
    new Monoid[List[A]] {
      def empty: List[A] = Nil
      def append(x: List[A], y: List[A]): List[A] = x ::: y
    }

  implicit def vector[A]: Monoid[Vector[A]] =
    new Monoid[Vector[A]] {
      def empty: Vector[A] = Vector.empty
      def append(x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
    }

  implicit def option[A](implicit A: Monoid[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def empty: Option[A] = None
      def append(x: Option[A], y: Option[A]): Option[A] =
        (x, y) match {
          case (None, None) => None
          case (_, None) => x
          case (None, _) => y
          case (Some(a), Some(b)) => Some(A.append(a, b))
        }
    }

  implicit def first[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def empty: Option[A] = None
      def append(x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
    }

  implicit def last[A]: Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def empty: Option[A] = None
      def append(x: Option[A], y: Option[A]): Option[A] = y.orElse(x)
    }

  implicit def map[K, V](implicit V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def empty: Map[K, V] = Map.empty
      def append(x: Map[K, V], y: Map[K, V]): Map[K, V] =
        x.foldLeft(y) {
          case (a, (k, v)) => a.updated(k, a.get(k).fold(v)(V.append(v, _)))
        }
    }

  implicit def set[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def empty: Set[A] = Set.empty
      def append(x: Set[A], y: Set[A]): Set[A] = x | y
    }

  implicit def endo[A]: Monoid[A => A] =
    new Monoid[A => A] {
      def empty: A => A = identity
      def append(f: A => A, g: A => A): A => A = f.andThen(g)
    }

  implicit def pair[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def empty: (A, B) = (A.empty, B.empty)
      def append(x: (A, B), y: (A, B)): (A, B) =
        (x, y) match {
          case ((ax, bx), (ay, by)) => (A.append(ax, ay), B.append(bx, by))
        }
    }

  implicit def triple[A, B, C](implicit A: Monoid[A], B: Monoid[B], C: Monoid[C]): Monoid[(A, B, C)] =
    new Monoid[(A, B, C)] {
      def empty: (A, B, C) = (A.empty, B.empty, C.empty)
      def append(x: (A, B, C), y: (A, B, C)): (A, B, C) =
        (x, y) match {
          case ((ax, bx, cx), (ay, by, cy)) => (A.append(ax, ay), B.append(bx, by), C.append(cx, cy))
        }
    }

  implicit def ordering[A]: Monoid[Ordering[A]] =
    new Monoid[Ordering[A]] {
      def empty: Ordering[A] =
        new Ordering[A] {
          def compare(a: A, b: A): Int = 0
        }
      def append(x: Ordering[A], y: Ordering[A]): Ordering[A] =
        new Ordering[A] {
          def compare(a: A, b: A): Int =
            x.compare(a, b) match {
              case 0 => y.compare(a, b)
              case n => n
            }
        }
    }

}
