package kits.lens

package example

import org.scalatest.FunSuite

class LensExample extends FunSuite {

  test("Optional") {
    case class Cons[+A](head: A, tail: Option[Cons[A]])
    val cons = Cons(1, Some(Cons(2, None)))
    assert(cons.withLens(_.head = 0) == Cons(0, Some(Cons(2, None))))
    assert(cons.withLens(_.tail = None) == Cons(1, None))
    assert(cons.withLens(_.tail.asOpt.head = 1) == Cons(1, Some(Cons(1, None))))
  }

  test("Prism") {
    sealed abstract class List[+A]
    case class Cons[+A](head: A, tail: List[A]) extends List[A]
    case object Nil extends List[Nothing]
    val cons = Cons(1, Cons(2, Nil))
    assert(cons.withLens(_.head = 0) == Cons(0, Cons(2, Nil)))
    assert(cons.withLens(_.tail = Nil) == Cons(1, Nil))
    assert(cons.withLens(_.tail.Cons.head = 1) == Cons(1, Cons(1, Nil)))
  }

}
