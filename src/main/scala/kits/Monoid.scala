package kits

trait Monoid[A] {
  def zero: A
  def append(x: A, y: A): A
}
