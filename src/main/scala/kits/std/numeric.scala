package kits

package std

object numeric {
  implicit def sum[A](implicit A: Numeric[A]) = new Monoid[A] {
    def zero: A = A.zero
    def append(x: A, y: A): A = A.plus(x, y)
  }
  implicit def product[A](implicit A: Numeric[A]) = new Monoid[A] {
    def zero: A = A.one
    def append(x: A, y: A): A = A.times(x, y)
  }
}
