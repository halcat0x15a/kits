package kits.mock

import org.mockito.Mockito
import org.mockito.exceptions.verification.WantedButNotInvoked
import org.scalatest.FunSuite

trait FooBar {
  def foo(i: Int)(s: String): Int
  val bar: String
  def baz: Unit
}

class MockExample extends FunSuite {

  test("returns") {
    val foobar: FooBar = mock[FooBar]
    val call = foobar.foo(0)("hoge") returns 1 returns 2
    assertThrows[WantedButNotInvoked](call.verify())
    assert(foobar.foo(0)("hoge") == 1)
    call.verify()
    assert(foobar.foo(0)("hoge") == 2)
    call.verify(Mockito.times(2))
  }

  test("overrides") {
    val foobar: FooBar = mock[FooBar]
    foobar overrides new {
      def foo(i: Int)(s: String) = i + 1
      val bar = "hoge"
      def baz = throw new RuntimeException
    }
    assert(foobar.foo(0)("hoge") == 1)
    assert(foobar.bar == "hoge")
    assertThrows[RuntimeException](foobar.baz)
    Mockito.verify(foobar).foo(0)("hoge")
    Mockito.verify(foobar).bar
    Mockito.verify(foobar).baz
  }

}
