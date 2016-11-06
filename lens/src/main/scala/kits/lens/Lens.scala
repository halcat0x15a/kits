package kits.lens

import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

abstract class Lens[A, B] extends Dynamic { self =>

  def get(a: A): B

  def set(b: B)(a: A): A

  final def modify(f: B => B)(a: A): A = set(f(get(a)))(a)

  final def compose[C](that: Lens[C, A]): Lens[C, B] =
    new Lens[C, B] {
      def get(c: C): B = self.get(that.get(c))
      def set(b: B)(c: C): C = that.set(self.set(b)(that.get(c)))(c)
    }

  final def andThen[C](that: Lens[B, C]): Lens[A, C] =
    new Lens[A, C] {
      def get(a: A): C = that.get(self.get(a))
      def set(c: C)(a: A): A = self.set(that.set(c)(self.get(a)))(a)
    }

  final def update(a: A, b: B): A = set(b)(a)

  final def selectDynamic(field: String): Any = macro Lens.selectDynamic[A, B]

  final def updateDynamic(field: String)(value: Any): Any = macro Lens.updateDynamic[A, B]

}

object Lens {

  def apply[A]: Lens[A, A] =
    new Lens[A, A] {
      def get(a: A): A = a
      def set(b: A)(a: A): A = b
    }

  def withLens[A](a: A)(fs: Lens[A, A] => A => A*): A = {
    val lens = Lens[A]
    fs.foldLeft(a)((a, f) => f(lens)(a))
  }

  def selectDynamic[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(field: c.Tree): c.Tree = {
    import c.universe._
    val Lens = symbolOf[Lens[_, _]]
    val name = field match { case Literal(Constant(name: String)) => TermName(name) }
    val A = weakTypeOf[A]
    val B = weakTypeOf[B]
    val C = B.member(name).infoIn(B)
    val prefix = c.prefix
    q"""
      new $Lens[$A, $C] {
        private[this] val prefix = $prefix
        def get(a: $A): $C = prefix.get(a).$name
        def set(c: $C)(a: $A): $A = prefix.set(prefix.get(a).copy($name = c))(a)
      }
    """
  }

  def updateDynamic[A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)(field: c.Tree)(value: c.Tree): c.Tree = {
    import c.universe._
    val A = weakTypeOf[A]
    val name = field match { case Literal(Constant(name: String)) => TermName(name) }
    val prefix = c.prefix
    q"{ (a: $A) => val prefix = $prefix; prefix.set(prefix.get(a).copy($name = $value))(a) }"
  }

}
