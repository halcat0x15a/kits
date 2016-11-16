package kits.lens

import scala.language.dynamics
import scala.language.experimental.macros

abstract class Prism[A, B] extends Dynamic { self =>

  def unapply(a: A): Option[B]

  def apply(b: B): A

  final def modifyOption(f: B => B)(a: A): Option[A] = unapply(a).map(b => apply(f(b)))

  final def modify(f: B => B)(a: A): A = modifyOption(f)(a).getOrElse(a)

  final def compose[C](that: Lens[C, A]): Optional[C, B] =
    new Optional[C, B] {
      def get(c: C): Option[B] = self.unapply(that.get(c))
      def set(b: B)(c: C): C = that.set(self.apply(b))(c)
    }

  final def andThen[C](that: Lens[B, C]): Optional[A, C] =
    new Optional[A, C] {
      def get(a: A): Option[C] = self.unapply(a).map(that.get)
      def set(c: C)(a: A): A = self.modify(that.set(c))(a)
    }

  final def compose[C](that: Prism[C, A]): Prism[C, B] =
    new Prism[C, B] {
      def unapply(c: C): Option[B] = that.unapply(c).flatMap(self.unapply)
      def apply(b: B): C = that.apply(self.apply(b))
    }

  final def andThen[C](that: Prism[B, C]): Prism[A, C] =
    new Prism[A, C] {
      def unapply(a: A): Option[C] = self.unapply(a).flatMap(that.unapply)
      def apply(c: C): A = self.apply(that.apply(c))
    }

  final def compose[C](that: Optional[C, A]): Optional[C, B] =
    new Optional[C, B] {
      def get(c: C): Option[B] = that.get(c).flatMap(self.unapply)
      def set(b: B)(c: C): C = that.set(self.apply(b))(c)
    }

  final def andThen[C](that: Optional[B, C]): Optional[A, C] =
    new Optional[A, C] {
      def get(a: A): Option[C] = self.unapply(a).flatMap(that.get)
      def set(c: C)(a: A): A = self.modify(that.set(c))(a)
    }

  final def selectDynamic(field: String): Any = macro LensMacros.selectDynamic[B]

}

object Prism {

  def apply[A]: Prism[A, A] =
    new Prism[A, A] {
      def unapply(a: A): Option[A] = Some(a)
      def apply(b: A): A = b
    }

}
