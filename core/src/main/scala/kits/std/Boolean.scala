package kits

package std

trait ConjMonoid extends Monoid[Boolean] {

  override final def empty: Boolean = true

  override final def append(x: Boolean, y: Boolean): Boolean = x && y

}

trait DisjMonoid extends Monoid[Boolean] {

  override final def empty: Boolean = false

  override final def append(x: Boolean, y: Boolean): Boolean = x || y

}
