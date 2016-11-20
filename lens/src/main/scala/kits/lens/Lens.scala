package kits.lens

import scala.language.dynamics
import scala.language.experimental.macros

abstract class Lens[A, B] extends Dynamic { self =>

  def get(a: A): B

  def set(b: B)(a: A): A

  final def modify(f: B => B)(a: A): A = set(f(get(a)))(a)

  final def compose[C](that: Lens[C, A]): Lens[C, B] =
    new Lens[C, B] {
      def get(c: C): B = self.get(that.get(c))
      def set(b: B)(c: C): C = that.modify(self.set(b))(c)
    }

  final def andThen[C](that: Lens[B, C]): Lens[A, C] =
    new Lens[A, C] {
      def get(a: A): C = that.get(self.get(a))
      def set(c: C)(a: A): A = self.modify(that.set(c))(a)
    }

  final def compose[C](that: Prism[C, A]): Optional[C, B] =
    new Optional[C, B] {
      def get(c: C): Option[B] = that.unapply(c).map(self.get)
      def set(b: B)(c: C): C = that.modify(self.set(b))(c)
    }

  final def andThen[C](that: Prism[B, C]): Optional[A, C] =
    new Optional[A, C] {
      def get(a: A): Option[C] = that.unapply(self.get(a))
      def set(c: C)(a: A): A = self.set(that.apply(c))(a)
    }

  final def compose[C](that: Optional[C, A]): Optional[C, B] =
    new Optional[C, B] {
      def get(c: C): Option[B] = that.get(c).map(self.get)
      def set(b: B)(c: C): C = that.modify(self.set(b))(c)
    }

  final def andThen[C](that: Optional[B, C]): Optional[A, C] =
    new Optional[A, C] {
      def get(a: A): Option[C] = that.get(self.get(a))
      def set(c: C)(a: A): A = self.modify(that.set(c))(a)
    }

  final def selectDynamic(field: String): Any = macro LensMacros.selectDynamic[B]

  final def updateDynamic(field: String)(value: Any): Any = macro LensMacros.updateDynamic[B]

}

object Lens {

  def apply[A]: Lens[A, A] =
    new Lens[A, A] {
      def get(a: A): A = a
      def set(b: A)(a: A): A = b
    }

}
