package kits

trait Monoid[A] extends Semigroup[A] {

  def empty: A

  implicit lazy val applicative: Applicative[({ type F[B] = A })#F] =
    new Applicative[({ type F[B] = A })#F] {
      def pure[B](b: B): A = empty
      def ap[B, C](fb: A)(f: A): A = append(f, fb)
    }

}

object Monoid {

  implicit val Conj: Monoid[Boolean] =
    new Monoid[Boolean] {
      val empty: Boolean = true
      def append(x: Boolean, y: Boolean): Boolean = x && y
    }

  implicit val Disj: Monoid[Boolean] =
    new Monoid[Boolean] {
      val empty: Boolean = false
      def append(x: Boolean, y: Boolean): Boolean = x || y
    }

  implicit def Sum[A](implicit A: Numeric[A]): Monoid[A] =
    new Monoid[A] {
      lazy val empty: A = A.zero
      def append(x: A, y: A): A = A.plus(x, y)
    }

  implicit def Prod[A](implicit A: Numeric[A]): Monoid[A] =
    new Monoid[A] {
      lazy val empty: A = A.one
      def append(x: A, y: A): A = A.times(x, y)
    }

}
