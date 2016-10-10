package kits.lens

import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

abstract class Lens[A, B] extends Dynamic { self =>

  def get(a: A): B

  def set(b: B)(a: A): A

  def modify(f: B => B)(a: A): A = set(f(get(a)))(a)

  def compose[C](that: Lens[C, A]): Lens[C, B] =
    new Lens[C, B] {
      def get(c: C): B = self.get(that.get(c))
      def set(b: B)(c: C): C = that.set(self.set(b)(that.get(c)))(c)
    }

  def andThen[C](that: Lens[B, C]): Lens[A, C] =
    new Lens[A, C] {
      def get(a: A): C = that.get(self.get(a))
      def set(c: C)(a: A): A = self.set(that.set(c)(self.get(a)))(a)
    }

  def selectDynamic(field: String): Any = macro Lens.selectDynamic[B]

  def updateDynamic(field: String)(value: Any): Any = macro Lens.updateDynamic[B]

}

object Lens {

  def apply[A]: Lens[A, A] =
    new Lens[A, A] {
      def get(a: A): A = a
      def set(b: A)(a: A): A = b
    }

  def withLens[A](a: A)(fs: Lens[A, A] => A => A*): A = fs.foldLeft(a)((a, f) => f(Lens[A])(a))

  def selectDynamic[A: c.WeakTypeTag](c: Context)(field: c.Tree): c.Tree = {
    import c.universe._
    val self = c.prefix.tree
    val that = makeLens[A](c)(field)
    q"$self.andThen($that)"
  }

  def updateDynamic[A: c.WeakTypeTag](c: Context)(field: c.Tree)(value: c.Tree): c.Tree = {
    import c.universe._
    val lens = selectDynamic[A](c)(field)
    q"$lens.set($value) _"
  }

  private[this] def makeLens[A: c.WeakTypeTag](c: Context)(field: c.Tree): c.Tree = {
    import c.universe._
    val Lens = symbolOf[Lens[_, _]]
    val name = field match { case Literal(Constant(name: String)) => TermName(name) }
    val A = weakTypeOf[A]
    val B = A.member(name).infoIn(A)
    q"""
      new $Lens[$A, $B] {
        def get(a: $A): $B = a.$name
        def set(b: $B)(a: $A): $A = a.copy($name = b)
      }
    """
  }

}
