package kits

import scala.language.experimental.macros

import generic._

case class Meta[T, A](a: A) {
  def tag = null.asInstanceOf[T]
}

trait Generic[A] {
  type Rep
  def from(a: A): Rep
  def to(r: Rep): A
}

object Generic {
  def apply[A]: Generic[A] = macro GenericMacros.apply[A]
}

private class GenericMacros(val c: scala.reflect.macros.whitebox.Context) {

  import c.universe._, internal._

  val left = symbolOf[Left[_, _]].companion

  val right = symbolOf[Right[_, _]].companion

  def apply[A: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[A]
    val sym = tpe.typeSymbol.asClass
    val rep = sumType(sym, tpe)
    val from = exprs(sym, false)
    val to = exprs(sym, true)
    q"""
new ${symbolOf[Generic[_]]}[$tpe] {
  type Rep = $rep
  def from(a: $tpe): Rep = a match { case ..$from }
  def to(r: Rep): $tpe = r match { case ..$to }
}
"""
  }

  def exprs(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    for ((a, r) <- constructorTree(sym, isExpr).zip(sumTree(sym, !isExpr))) yield
      if (isExpr)
        cq"$r => $a"
      else
        cq"$a => $r"

  def sumType(sym: ClassSymbol, tpe: Type): Type =
    metaType(sym, constructors(sym).map(productType(_, tpe)).foldRight(typeOf[Void])(appliedType(typeOf[:+:[_, _]], _, _)))

  def productType(sym: ClassSymbol, tpe: Type): Type =
    metaType(sym, parameterTypes(sym, tpe).foldRight(typeOf[Unit])(appliedType(typeOf[:*:[_, _]], _, _)))

  def parameterTypes(sym: ClassSymbol, tpe: Type): List[Type] =
    parameters(sym).map(param => metaType(param, param.info.substituteTypes(sym.typeParams, tpe.typeArgs)))

  def metaType(sym: Symbol, tpe: Type): Type =
    appliedType(typeOf[Meta[_, _]], constantType(Constant(sym.name.toString)), tpe)

  def sumTree(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    constructors(sym).foldRight(List(q"${symbolOf[Void]}"))((a, b) => q"$left(${productTree(a, isExpr)})" :: b.map(x => q"$right($x)")).map(metaTree)

  def productTree(ctor: ClassSymbol, isExpr: Boolean): Tree =
    metaTree(parameterTrees(ctor, isExpr).foldRight(q"${symbolOf[Unit].companion}")((a, b) => q"${symbolOf[:*:[_, _]].companion}(${metaTree(a)}, $b)"))

  def metaTree(tree: Tree): Tree =
    q"${symbolOf[Meta[_, _]].companion}($tree)"

  def constructorTree(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    for (ctor <- constructors(sym)) yield
      if (ctor.isModuleClass)
        q"${ctor.module}"
      else
        q"${ctor.companion}(..${parameterTrees(ctor, isExpr)})"

  def parameterTrees(ctor: ClassSymbol, isExpr: Boolean): List[Tree] =
    for {
      param <- parameters(ctor)
      name = param.asTerm.name
    } yield if (isExpr) q"$name" else pq"$name@_"

  def constructors(sym: ClassSymbol): List[ClassSymbol] = {
    sym.typeSignature
    if (sym.isSealed)
      sym.knownDirectSubclasses.toList.flatMap(sym => constructors(sym.asClass))
    else if (sym.isCaseClass)
      List(sym)
    else
      Nil
  }

  def parameters(ctor: ClassSymbol): List[Symbol] =
    ctor.primaryConstructor.asMethod.paramLists.head

}
