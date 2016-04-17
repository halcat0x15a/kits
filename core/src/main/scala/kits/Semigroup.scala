package kits

import scala.language.implicitConversions

trait Semigroup[A] { A =>

  def append(x: A, y: A): A

  class SemigroupOps(self: A) {

    def append(xs: A*): A = xs.foldLeft(self)(A.append)

  }

}

object Semigroup {

  object Implicits {

    implicit def SemigroupOps[A](self: A)(implicit A: Semigroup[A]): Semigroup[A]#SemigroupOps = new A.SemigroupOps(self)

  }

  implicit val String: Monoid[String] = new Monoid.StringMonoid {}

  implicit val Unit: Monoid[Unit] = new Monoid.UnitMonoid {}

  implicit def Tuple2[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid.Tuple2Monoid[A, B] {
      val _1 = A
      val _2 = B
    }

  implicit def Tuple3[A, B, C](implicit A: Monoid[A], B: Monoid[B], C: Monoid[C]): Monoid[(A, B, C)] =
    new Monoid.Tuple3Monoid[A, B, C] {
      val _1 = A
      val _2 = B
      val _3 = C
    }

  implicit def Option[A](implicit A: Semigroup[A]): Monoid[Option[A]] =
    new Monoid.OptionMonoid[A] {
      val semigroup = A
    }

  implicit def Map[K, V](implicit V: Semigroup[V]): Monoid[Map[K, V]] =
    new Monoid.MapMonoid[K, V] {
      val semigroup = V
    }

  implicit def Min[A](implicit A: Ordering[A]): Semigroup[A] =
    new Semigroup.MinSemigroup[A] {
      val ordering = A
    }

  implicit def Max[A](implicit A: Ordering[A]): Semigroup[A] =
    new Semigroup.MaxSemigroup[A] {
      val ordering = A
    }

  implicit def Ordering[A]: Monoid[Ordering[A]] = new Monoid.OrderingMonoid[A] {}

  implicit def MonadPlus[A](implicit A: Unify[MonadPlus, A]): Monoid[A] =
    new Monoid[A] {
      lazy val empty: A = A.from(A.TC.zero)
      def append(x: A, y: A): A = A.from(A.TC.plus(A.to(x), A.to(y)))
    }

  trait MinSemigroup[A] extends Semigroup[A] {

    def ordering: Ordering[A]

    override final def append(x: A, y: A): A = ordering.min(x, y)

  }

  trait MaxSemigroup[A] extends Semigroup[A] {

    def ordering: Ordering[A]

    override final def append(x: A, y: A): A = ordering.max(x, y)

  }

}
