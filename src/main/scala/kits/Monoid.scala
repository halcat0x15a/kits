package kits

trait Monoid[A] { A =>

  def empty: A

  def append(x: A, y: A): A

  def multiply(a: A, n: Int): A = {
    @scala.annotation.tailrec
    def go(acc: A, n: Int): A =
      if (n <= 0)
        empty
      else if (n == 1)
        acc
      else
        go(append(acc, a), n - 1)
    go(a, n)
  }

  lazy val applicative: Applicative[({ type F[B] = A })#F] =
    new Applicative[({ type F[B] = A })#F] {
      def pure[B](b: B): A = A.empty
      def ap[B, C](fb: A)(f: A): A = A.append(f, fb)
    }

  lazy val dual: Monoid[A] =
    new Monoid[A] {
      def empty: A = A.empty
      def append(x: A, y: A): A = A.append(y, x)
    }

}

object Monoid {

  def apply[A](implicit A: Monoid[A]): Monoid[A] = A

  def append[A](xs: A*)(implicit A: Monoid[A]): A = xs.foldLeft(A.empty)(A.append)

  def multiply[A](a: A, n: Int)(implicit A: Monoid[A]): A = A.multiply(a, n)

  implicit def sum[A](implicit A: Numeric[A]): Monoid[Sum[A]] =
    new Monoid[Sum[A]] {
      def empty: Sum[A] = Sum(A.zero)
      def append(x: Sum[A], y: Sum[A]): Sum[A] = x.append(y)
    }

  implicit def product[A](implicit A: Numeric[A]): Monoid[Prod[A]] =
    new Monoid[Prod[A]] {
      def empty: Prod[A] = Prod(A.one)
      def append(x: Prod[A], y: Prod[A]): Prod[A] = x.append(y)
    }

  implicit val conj: Monoid[Conj] =
    new Monoid[Conj] {
      def empty: Conj = Conj(true)
      def append(x: Conj, y: Conj): Conj = x.append(y)
    }

  implicit val disj: Monoid[Disj] =
    new Monoid[Disj] {
      def empty: Disj = Disj(false)
      def append(x: Disj, y: Disj): Disj = x.append(y)
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

  implicit def first[A]: Monoid[First[A]] =
    new Monoid[First[A]] {
      def empty: First[A] = First(None)
      def append(x: First[A], y: First[A]): First[A] = x.append(y)
    }

  implicit def last[A]: Monoid[Last[A]] =
    new Monoid[Last[A]] {
      def empty: Last[A] = Last(None)
      def append(x: Last[A], y: Last[A]): Last[A] = x.append(y)
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

  implicit def endo[A]: Monoid[Endo[A]] =
    new Monoid[Endo[A]] {
      def empty: Endo[A] = Endo(identity)
      def append(f: Endo[A], g: Endo[A]): Endo[A] = f.append(g)
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
