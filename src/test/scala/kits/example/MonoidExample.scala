package kits

package example

import org.scalatest.FunSuite

import Monoid.{Sum, Product}

class MonoidExample extends FunSuite {

  trait Monoid[A] {
    def empty: A
    def append(x: A, y: A): A
  }

  implicit val intMonoid: Monoid[Int] =
    new Monoid[Int] {
      def empty: Int = 0
      def append(x: Int, y: Int): Int = x + y
    }

  implicit val stringMonoid: Monoid[String] =
    new Monoid[String] {
      def empty: String = ""
      def append(x: String, y: String): String = x + y
    }

  def double[A](a: A)(implicit A: Monoid[A]): A = A.append(a, a)

  test("double") {
    assert(double("hoge") == "hogehoge")
    assert(double("fuga") == "fugafuga")
    assert(double(2) == 4)
    assert(double(3) == 6)
  }

  test("append") {
    assert(kits.Monoid.append(List(0, 1), List(2, 3)) == List(0, 1, 2, 3))
    assert(kits.Monoid.append("foo", "bar", "baz") == "foobarbaz")
    assert(kits.Monoid.append(Sum(2), Sum(3)) == Sum(5))
    assert(kits.Monoid.append(Product(2), Product(3)) == Product(6))
    assert(kits.Monoid.append(Some("foo"), None, Some("bar")) == Some("foobar"))
    assert(kits.Monoid.append(Map('a -> "foo", 'b -> "bar"), Map('a -> "bar", 'c -> "baz")) == Map('a -> "foobar", 'b -> "bar", 'c -> "baz"))
  }

}
