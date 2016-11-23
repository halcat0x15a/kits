package kits

package std

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
