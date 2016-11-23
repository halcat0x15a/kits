package kits

package std

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
