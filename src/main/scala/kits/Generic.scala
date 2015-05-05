package kits

import scala.language.experimental.macros

trait Generic[A] {
  type Rep
  def to(rep: Rep): A
  def from(a: A): Rep
}

object Generic {
  def derive[A]: Generic[A] = macro GenericMacros.derive[A]
  private class GenericMacros(val c: scala.reflect.macros.whitebox.Context) {
    import c.universe._
    def derive[A: c.WeakTypeTag]: Tree = {
      val a = weakTypeOf[A]
      val xs = exprs(a)
      q"""new Generic[$a] {
            type Rep = ${rep(a)}
            def from(a: $a): Rep = ${convert(q"a", xs)}
            def to(r: Rep): $a = ${convert(q"r", xs.map(_.swap))}
          }"""
    }
    def ctors(s: Symbol): List[ClassSymbol] = {
      val c = s.asClass
      c.typeSignature
      if (c.isSealed)
        c.knownDirectSubclasses.toList.flatMap(ctors)
      else if (c.isCaseClass)
        List(c)
      else
        Nil
    }
    def params(c: ClassSymbol, t: Type): List[(TermName, Type)] = {
      val ctor = c.primaryConstructor
      if (ctor.isMethod)
        for (param <- ctor.asMethod.paramLists.head) yield
          param.asTerm.name -> param.info.substituteTypes(c.typeParams, t.typeArgs)
      else
        Nil
    }
    def rep(t: Type): Tree =
      ctors(t.typeSymbol).map(c => params(c, t).map(_._2) match {
        case Nil => tq"Unit"
        case t :: ts => ts.foldLeft(tq"$t")((a, b) => tq"($a, $b)")
      }) match {
        case Nil => tq"$t"
        case t :: ts => ts.foldLeft(t)((a, b) => tq"Either[$a, $b]")
      }
    def exprs(t: Type): List[(Tree, Tree)] =
      ctors(t.typeSymbol).map(c =>
        if (c.isModuleClass)
          q"${c.module}"
        else
          q"${c.companion}(..${params(c, t).map(_._1)})"
      ).zip(ctors(t.typeSymbol).map(c => params(c, t).map(_._1) match {
        case Nil => q"()"
        case t :: ts => ts.foldLeft(q"$t": Tree)((a, b) => q"($a, $b)")
      }) match {
        case Nil => Nil
        case t :: ts => ts.foldLeft(Vector(t))((a, b) => a.map(t => q"Left($t)") :+ q"Right($b)")
      })
    def bind(t: Tree): Tree =
      t match {
        case q"$c(..$ts)" => pq"$c(..${ts.map(bind)})"
        case q"${i: TermName}" if t.symbol == NoSymbol => pq"$i@_"
        case _ => t
      }
    def convert(t: Tree, exprs: List[(Tree, Tree)]) =
      exprs.map {
        case (p, e) => cq"${bind(p)} => $e"
      } match {
        case Nil => t
        case cs => q"$t match { case ..$cs }"
      }
  }
}
