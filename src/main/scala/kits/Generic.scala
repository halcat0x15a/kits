package kits

import scala.language.experimental.macros

import generic._

trait Generic[A] {

  type Rep

  def from(a: A): Rep

  def to(r: Rep): A

}

object Generic {

  def apply[A]: Generic[A] = macro GenericMacros.apply[A]

}

private class GenericMacros(val c: scala.reflect.macros.whitebox.Context) {

  import c.universe._

  def apply[A: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[A]
    val sym = tpe.typeSymbol.asClass
    val rep = sumType(sym, tpe)
    val from = cases(sym, false)
    val to = cases(sym, true)
    q"""
new ${symbolOf[Generic[_]]}[$tpe] {
  type Rep = $rep
  def from(a: $tpe): Rep = a match { case ..$from }
  def to(r: Rep): $tpe = (r: @${typeOf[unchecked]}) match { case ..$to }
}
"""
  }

  def cases(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    for ((a, r) <- constructorTree(sym, isExpr).zip(sumTree(sym, !isExpr))) yield
      if (isExpr)
        cq"$r => $a"
      else
        cq"$a => $r"

  def sumType(sym: ClassSymbol, tpe: Type): Type =
    constructors(sym).map(productType(_, tpe)).foldRight(typeOf[Void])(appliedType(typeOf[:+:[_, _]], _, _))

  def sumTree(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    constructors(sym).foldRight(List(q"${symbolOf[Void].companion}")) { (a, b) =>
      q"${symbolOf[Left[_, _]].companion}(${productTree(a, isExpr)})" :: b.map(x => q"${symbolOf[Right[_, _]].companion}($x)")
    }

  def productType(sym: ClassSymbol, tpe: Type): Type =
    parameters(sym).map(_.info.substituteTypes(sym.typeParams, tpe.typeArgs)).foldRight(typeOf[Unit])(appliedType(typeOf[:*:[_, _]], _, _))

  def productTree(sym: ClassSymbol, isExpr: Boolean): Tree =
    parameterTree(sym, isExpr).foldRight(q"${symbolOf[Unit].companion}") { (a, b) =>
      q"${symbolOf[:*:[_, _]].companion}($a, $b)"
    }

  def constructorTree(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
    for (ctor <- constructors(sym)) yield
      if (ctor.isModuleClass)
        q"${ctor.module}"
      else
        q"${ctor.companion}(..${parameterTree(ctor, isExpr)})"

  def parameterTree(sym: ClassSymbol, isExpr: Boolean): List[Tree] =
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
