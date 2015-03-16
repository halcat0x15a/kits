package kits

import org.scalacheck.Arbitrary

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

abstract class TraverseSuite[F[_], G[_], A](implicit F: Traverse[F], G: Applicative[G], FA: Arbitrary[F[A]], GA: Arbitrary[G[A]]) extends FunSuite with Checkers {
  test("identity") {
    check { fa: F[A] =>
      F.traverse[Id, A, A](fa)(identity) == fa
    }
  }
  test("composition") {
    check { (fa: F[A], f: A => G[A], g: A => G[A]) =>
      F.traverse[({ type H[A] = Comp[G, G, A] })#H, A, A](fa)(f.andThen(G.map(_)(g))) == G.map(F.traverse(fa)(f))(F.traverse(_)(g))
    }
  }
}

class IdTraverseSuite extends TraverseSuite[Id, Option, AnyVal]

class ListTraverseSuite extends TraverseSuite[List, Option, AnyVal]

class VectorTraverseSuite extends TraverseSuite[Vector, Option, AnyVal]

class OptionTraverseSuite extends TraverseSuite[Option, Option, AnyVal]

class MapTraverseSuite extends TraverseSuite[({ type F[A] = Map[AnyVal, A] })#F, Option, AnyVal]

class SetTraverseSuite extends TraverseSuite[Set, Option, AnyVal]

class RightTraverseSuite extends TraverseSuite[({ type F[A] = Either[AnyVal, A] })#F, Option, AnyVal]

class LeftTraverseSuite extends TraverseSuite[({ type F[A] = Either[A, AnyVal] })#F, Option, AnyVal]
