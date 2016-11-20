package kits.lens

import scala.language.dynamics
import scala.language.experimental.macros

abstract class Optional[A, B] extends Dynamic { self =>

  def get(a: A): Option[B]

  def set(b: B)(a: A): A

  final def modifyOption(f: B => B)(a: A): Option[A] = get(a).map(b => set(f(b))(a))

  final def modify(f: B => B)(a: A): A = modifyOption(f)(a).getOrElse(a)

  final def compose[C](that: Lens[C, A]): Optional[C, B] =
    new Optional[C, B] {
      def get(c: C): Option[B] = self.get(that.get(c))
      def set(b: B)(c: C): C = that.modify(self.set(b))(c)
    }

  final def andThen[C](that: Lens[B, C]): Optional[A, C] =
    new Optional[A, C] {
      def get(a: A): Option[C] = self.get(a).map(that.get)
      def set(c: C)(a: A): A = self.modify(that.set(c))(a)
    }

  final def compose[C](that: Prism[C, A]): Optional[C, B] =
    new Optional[C, B] {
      def get(c: C): Option[B] = that.unapply(c).flatMap(self.get)
      def set(b: B)(c: C): C = that.modify(self.set(b))(c)
    }

  final def andThen[C](that: Prism[B, C]): Optional[A, C] =
    new Optional[A, C] {
      def get(a: A): Option[C] = self.get(a).flatMap(that.unapply)
      def set(c: C)(a: A): A = self.set(that.apply(c))(a)
    }

  final def compose[C](that: Optional[C, A]): Optional[C, B] =
    new Optional[C, B] {
      def get(c: C): Option[B] = that.get(c).flatMap(self.get)
      def set(b: B)(c: C): C = that.modify(self.set(b))(c)
    }

  final def andThen[C](that: Optional[B, C]): Optional[A, C] =
    new Optional[A, C] {
      def get(a: A): Option[C] = self.get(a).flatMap(that.get)
      def set(c: C)(a: A): A = self.modify(that.set(c))(a)
    }

  final def selectDynamic(field: String): Any = macro LensMacros.selectDynamic[B]

  final def updateDynamic(field: String)(value: Any): Any = macro LensMacros.updateDynamic[B]

}
