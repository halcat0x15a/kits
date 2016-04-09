package kits

trait Monoid[A] extends Semigroup[A] {

  def empty: A

}
