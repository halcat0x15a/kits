package kits

package std

trait StringMonoid extends Monoid[String] {

  override final def empty: String = ""

  override final def append(x: String, y: String): String = x + y

}
