package kits

import scala.reflect.ClassTag

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

import kits.std._

abstract class MonoidLaw[A](implicit A: Monoid[A], fa: Arbitrary[A], tag: ClassTag[A]) extends Properties(s"Monoid[${tag.runtimeClass.getSimpleName}]") {
  property("rightIdentity") = forAll { a: A =>
    A.append(a, A.zero) == a
  }
  property("leftIdentity") = forAll { a: A =>
    A.append(A.zero, a) == a
  }
}

object StringMonoidSpec extends MonoidLaw[String]

object UnitMonoidSpec extends MonoidLaw[Unit]
