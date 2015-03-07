package kits

package std

object boolean {
  implicit val all = new Monoid[Boolean] {
    def zero: Boolean = true
    def append(x: Boolean, y: Boolean): Boolean = x && y
  }
  implicit val any = new Monoid[Boolean] {
    def zero: Boolean = false
    def append(x: Boolean, y: Boolean): Boolean = x || y
  }
}
