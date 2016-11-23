package kits

package std

trait UnitMonoid extends Monoid[Unit] {

  override final def empty: Unit = ()

  override final def append(x: Unit, y: Unit): Unit = ()

}
