package kits

trait Monoid[A] extends Semigroup[A] {

  def empty: A

  implicit def applicative: Applicative[({ type F[B] = A })#F] =
    new Applicative[({ type F[B] = A })#F] {
      def pure[B](b: B): A = empty
      def ap[B, C](fb: A)(f: A): A = append(f, fb)
    }

}

object Monoid {

  implicit val Conj: Monoid[Boolean] = new Monoid.ConjMonoid {}

  implicit val Disj: Monoid[Boolean] = new Monoid.DisjMonoid {}

  implicit def Sum[A](implicit A: Numeric[A]): Monoid[A] =
    new SumMonoid[A] {
      val numeric = A
    }

  implicit def Prod[A](implicit A: Numeric[A]): Monoid[A] =
    new ProdMonoid[A] {
      val numeric = A
    }

  trait StringMonoid extends Monoid[String] {

    override final def empty: String = ""

    override final def append(x: String, y: String): String = x + y

  }

  trait UnitMonoid extends Monoid[Unit] {

    override final def empty: Unit = ()

    override final def append(x: Unit, y: Unit): Unit = ()

  }

  trait ConjMonoid extends Monoid[Boolean] {

    override final def empty: Boolean = true

    override final def append(x: Boolean, y: Boolean): Boolean = x && y

  }

  trait DisjMonoid extends Monoid[Boolean] {

    override final def empty: Boolean = false

    override final def append(x: Boolean, y: Boolean): Boolean = x || y

  }

  trait SumMonoid[A] extends Monoid[A] {

    def numeric: Numeric[A]

    override final def empty: A = numeric.zero

    override final def append(x: A, y: A): A = numeric.plus(x, y)

  }

  trait ProdMonoid[A] extends Monoid[A] {

    def numeric: Numeric[A]

    override final def empty: A = numeric.one

    override final def append(x: A, y: A): A = numeric.times(x, y)

  }

  trait Tuple2Monoid[A, B] extends Monoid[(A, B)] {

    def _1: Monoid[A]

    def _2: Monoid[B]

    override final def empty: (A, B) = (_1.empty, _2.empty)

    override final def append(x: (A, B), y: (A, B)): (A, B) =
      (x, y) match {
        case ((ax, bx), (ay, by)) => (_1.append(ax, ay), _2.append(bx, by))
      }

  }

  trait Tuple3Monoid[A, B, C] extends Monoid[(A, B, C)] {

    def _1: Monoid[A]

    def _2: Monoid[B]

    def _3: Monoid[C]

    override final def empty: (A, B, C) = (_1.empty, _2.empty, _3.empty)

    override final def append(x: (A, B, C), y: (A, B, C)): (A, B, C) =
      (x, y) match {
        case ((ax, bx, cx), (ay, by, cy)) => (_1.append(ax, ay), _2.append(bx, by), _3.append(cx, cy))
      }

  }

  trait OptionMonoid[A] extends Monoid[Option[A]] {

    def semigroup: Semigroup[A]

    override final def empty: Option[A] = None

    override final def append(x: Option[A], y: Option[A]): Option[A] =
      (x, y) match {
        case (None, None) => None
        case (_, None) => x
        case (None, _) => y
        case (Some(a), Some(b)) => Some(semigroup.append(a, b))
      }

  }

  trait MapMonoid[K, V] extends Monoid[Map[K, V]] {

    def semigroup: Semigroup[V]

    override final def empty: Map[K, V] = Map.empty

    override final def append(x: Map[K, V], y: Map[K, V]): Map[K, V] =
      x.foldLeft(y) {
        case (a, (k, v)) => a.updated(k, a.get(k).fold(v)(semigroup.append(v, _)))
      }

  }

  trait OrderingMonoid[A] extends Monoid[Ordering[A]] {

    override final def empty: Ordering[A] =
      new Ordering[A] {
        def compare(a: A, b: A): Int = 0
      }

    override final def append(x: Ordering[A], y: Ordering[A]): Ordering[A] =
      new Ordering[A] {
        def compare(a: A, b: A): Int =
          x.compare(a, b) match {
            case 0 => y.compare(a, b)
            case n => n
          }
      }

  }

}
