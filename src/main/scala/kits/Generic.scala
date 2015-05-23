package kits

import scala.language.experimental.macros

import generic._

case class Meta[T, A](value: A)(implicit val tag: scala.reflect.ClassTag[T])

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

  def apply[A: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[A]
    val sym = tpe.typeSymbol.asClass
    val rep = sum(sym, tpe)
    val from = cases(sym, false)
    val to = cases(sym, true)
    q"""
new ${symbolOf[Generic[_]]}[$tpe] {
  type Rep = $rep
  def from(a: $tpe): Rep = a match { case ..$from }
  def to(r: Rep): $tpe = r match { case ..$to }
}
"""
  }

  def cases(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    for ((a, r) <- constructor(sym, isExpr).zip(sum(sym, !isExpr))) yield
      if (isExpr)
        cq"$r => $a"
      else
        cq"$a => $r"

  def sum(sym: ClassSymbol, tpe: Type): Type =
    meta(tpe, constructors(sym).map(product(_, tpe)).foldRight(typeOf[Void])(appliedType(typeOf[:+:[_, _]], _, _)))

  def sum(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    constructors(sym).foldRight(List(q"${symbolOf[Void].companion}"))((a, b) => q"${symbolOf[Left[_, _]].companion}(${product(a, isExpr)})" :: b.map(x => q"${symbolOf[Right[_, _]].companion}($x)")).map(meta)

  def product(sym: ClassSymbol, tpe: Type): Type =
    meta(sym.selfType.substituteTypes(sym.typeParams, tpe.typeArgs), parameters(sym).map(_.info.substituteTypes(sym.typeParams, tpe.typeArgs)).foldRight(typeOf[Unit])(appliedType(typeOf[:*:[_, _]], _, _)))

  def product(sym: ClassSymbol, isExpr: Boolean): Tree =
    meta(parameter(sym, isExpr).foldRight(q"${symbolOf[Unit].companion}")((a, b) => q"${symbolOf[:*:[_, _]].companion}($a, $b)"))

  def meta(tag: Type, value: Type): Type =
    appliedType(typeOf[Meta[_, _]], tag, value)

  def meta(tree: Tree): Tree =
    q"${symbolOf[Meta[_, _]].companion}($tree)"

  def constructor(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    for (ctor <- constructors(sym)) yield
      if (ctor.isModuleClass)
        q"${ctor.module}"
      else
        q"${ctor.companion}(..${parameter(ctor, isExpr)})"

  def parameter(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    for {
      param <- parameters(sym)
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

  def parameters(sym: ClassSymbol): List[Symbol] =
    sym.primaryConstructor.asMethod.paramLists.head

}
