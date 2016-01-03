package kits

package example

import org.scalatest.FunSuite

class MonoidExample extends FunSuite {

  test("double") {
    trait Monoid[A] {
      def empty: A
      def append(x: A, y: A): A
    }
    def double[A](a: A)(implicit A: Monoid[A]): A = A.append(a, a)
    implicit val intMonoid: Monoid[Int] =
      new Monoid[Int] {
        def empty: Int = 0
        def append(x: Int, y: Int): Int = x + y
      }
    assert(double(2) == 4)
    assert(double(3) == 6)
    implicit val stringMonoid: Monoid[String] =
      new Monoid[String] {
        def empty: String = ""
        def append(x: String, y: String): String = x + y
      }
    assert(double("hoge") == "hogehoge")
    assert(double("fuga") == "fugafuga")
  }

  test("append") {
    assert(kits.Monoid.append(List(0, 1), List(2, 3)) == List(0, 1, 2, 3))
    assert(kits.Monoid.append("foo", "bar") == "foobar")
    assert(kits.Monoid.append(Sum(2), Sum(3)) == 5)
    assert(kits.Monoid.append(Prod(2), Prod(3)) == 6)
    assert(kits.Monoid.append(Some("foo"), None) == Some("foo"))
    assert(kits.Monoid.append(Map('a -> "foo", 'b -> "bar"), Map('a -> "bar", 'c -> "baz")) == Map('a -> "foobar", 'b -> "bar", 'c -> "baz"))
  }

}
