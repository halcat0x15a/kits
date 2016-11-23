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
    assert(Monoid.append(List(0, 1), List(2, 3)) == List(0, 1, 2, 3))
    assert(Traverse.fold(List("foo", "bar", "baz")) == "foobarbaz")
    assert(Traverse.fold(List(Option("foo"), None, Some("bar"))) == Some("foobar"))
    assert(Monoid.append(Map('a -> "foo", 'b -> "bar"), Map('a -> "bar", 'c -> "baz")) == Map('a -> "foobar", 'b -> "bar", 'c -> "baz"))
  }

}
