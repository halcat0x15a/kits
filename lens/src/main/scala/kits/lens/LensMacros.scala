package kits.lens

import scala.reflect.macros.whitebox.Context

class LensMacros(val c: Context) {

  import c.universe._

  def makePrism[A: WeakTypeTag](name: TypeName): Tree = {
    val Prism = symbolOf[Prism[_, _]]
    val A = weakTypeOf[A]
    val subclass = A.typeSymbol.asClass.knownDirectSubclasses
      .find(_.name == name)
      .getOrElse(NoSymbol)
      .asClass
    val typeArgs = subclass.toType.baseType(A.typeSymbol).typeArgs.map(_.typeSymbol).zip(A.typeArgs).toMap
    val B = appliedType(subclass, subclass.typeParams.map(param => typeArgs.get(param).getOrElse(typeOf[Any])))
    q"""
      new $Prism[$A, $B] {
        def unapply(a: $A): Option[$B] = a match {
          case b: $B => Some(b)
          case _ => None
        }
        def apply(b: $B): $A = b
      }
    """
  }

  def makeLens[A: WeakTypeTag](name: TermName): Tree = {
    val Lens = symbolOf[Lens[_, _]]
    val A = weakTypeOf[A]
    val B = A.member(name).infoIn(A)
    q"""
      new $Lens[$A, $B] {
        def get(a: $A): $B = a.$name
        def set(b: $B)(a: $A): $A = a.copy($name = b)
      }
    """
  }

  def selectDynamic[A: WeakTypeTag](field: Tree): Tree = {
    val A = symbolOf[A]
    val self = c.prefix
    val Literal(Constant(name: String)) = field
    val that = if (A.asClass.isSealed) makePrism(TypeName(name)) else makeLens(TermName(name))
    q"$self.andThen($that)"
  }

  def applyDynamic[A: WeakTypeTag](field: Tree)(value: Tree): Tree = {
    val lens = selectDynamic[A](field)
    q"$lens.get($value)"
  }

  def updateDynamic[A: WeakTypeTag](field: Tree)(value: Tree): Tree = {
    val lens = selectDynamic[A](field)
    q"$lens.set($value) _"
  }

}
