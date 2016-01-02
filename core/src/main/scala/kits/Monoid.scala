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

  def append[A](x: A, y: A)(implicit A: Monoid[A]): A = A.append(x, y)

  def multiply[A](a: A, n: Int)(implicit A: Monoid[A]): A = A.multiply(a, n)

  implicit val conj: Monoid[Boolean] =
    new Monoid[Boolean] {
      lazy val empty: Boolean = true
      def append(x: Boolean, y: Boolean): Boolean = x && y
    }

  implicit val disj: Monoid[Boolean] =
    new Monoid[Boolean] {
      lazy val empty: Boolean = false
      def append(x: Boolean, y: Boolean): Boolean = x || y
    }

  implicit val string: Monoid[String] =
    new Monoid[String] {
      lazy val empty: String = ""
      def append(x: String, y: String): String = x + y
    }

  implicit val unit: Monoid[Unit] =
    new Monoid[Unit] {
      lazy val empty: Unit = ()
      def append(x: Unit, y: Unit): Unit = ()
    }

  implicit def pair[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      lazy val empty: (A, B) = (A.empty, B.empty)
      def append(x: (A, B), y: (A, B)): (A, B) =
        (x, y) match {
          case ((ax, bx), (ay, by)) => (A.append(ax, ay), B.append(bx, by))
        }
    }

  implicit def triple[A, B, C](implicit A: Monoid[A], B: Monoid[B], C: Monoid[C]): Monoid[(A, B, C)] =
    new Monoid[(A, B, C)] {
      lazy val empty: (A, B, C) = (A.empty, B.empty, C.empty)
      def append(x: (A, B, C), y: (A, B, C)): (A, B, C) =
        (x, y) match {
          case ((ax, bx, cx), (ay, by, cy)) => (A.append(ax, ay), B.append(bx, by), C.append(cx, cy))
        }
    }

  implicit def option[A](implicit A: Monoid[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      lazy val empty: Option[A] = None
      def append(x: Option[A], y: Option[A]): Option[A] =
        (x, y) match {
          case (None, None) => None
          case (_, None) => x
          case (None, _) => y
          case (Some(a), Some(b)) => Some(A.append(a, b))
        }
    }

  implicit def map[K, V](implicit V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      lazy val empty: Map[K, V] = Map.empty
      def append(x: Map[K, V], y: Map[K, V]): Map[K, V] =
        x.foldLeft(y) {
          case (a, (k, v)) => a.updated(k, a.get(k).fold(v)(V.append(v, _)))
        }
    }

  implicit def sum[A](implicit A: Numeric[A]): Monoid[A] =
    new Monoid[A] {
      lazy val empty: A = A.zero
      def append(x: A, y: A): A = A.plus(x, y)
    }

  implicit def prod[A](implicit A: Numeric[A]): Monoid[A] =
    new Monoid[A] {
      lazy val empty: A = A.one
      def append(x: A, y: A): A = A.times(x, y)
    }

  implicit def ordering[A]: Monoid[Ordering[A]] =
    new Monoid[Ordering[A]] {
      lazy val empty: Ordering[A] =
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

  implicit def monadPlus[F[_], A](implicit F: MonadPlus[F]): Monoid[F[A]] = F.monoid

}
