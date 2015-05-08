package kits

import scala.language.experimental.macros

sealed trait U
case object U extends U {
  implicit def functor[A0] = new Unapply[Functor, U] {
    type F[_] = U
    type A = A0
    def apply(fa: U) = U
    val T = new Functor[F] {
      def map[A, B](fa: U)(f: A => B) = U
    }
  }
}

case class V[A](a: A)
object V {
  implicit val functor = new Functor[V] {
    def map[A, B](fa: V[A])(f: A => B) = fa match {
      case V(a) => V(f(a))
    }
  }
}

case class :*:[A, B](a: A, b: B)

object :*: {
  implicit def functor[FA, GA, A0](implicit FA: Unapply[Functor, FA] { type A = A0 }, GA: Unapply[Functor, GA] { type A = A0 }) = new Unapply[Functor, FA :*: GA] {
    type F[A] = FA.F[A] :*: GA.F[A]
    type A = A0
    def apply(fa: FA :*: GA) = fa match {
      case a :*: b => :*:(FA(a), GA(b))
    }
    val T = new Functor[F] {
      def map[A, B](fa: FA.F[A] :*: GA.F[A])(f: A => B) = fa match {
        case a :*: b => :*:(FA.T.map(a)(f), GA.T.map(b)(f))
      }
    }
  }
}

sealed trait :+:[+A, +B]
case class L[A](a: A) extends (A :+: Nothing)
case class R[B](b: B) extends (Nothing :+: B)

object :+: {
  implicit def functor[FA, GA, A0](implicit FA: Unapply[Functor, FA] { type A = A0 }, GA: Unapply[Functor, GA] { type A = A0 }) = new Unapply[Functor, FA :+: GA] {
    type F[A] = FA.F[A] :+: GA.F[A]
    type A = A0
    def apply(fa: FA :+: GA) = fa match {
      case L(a) => L(FA(a))
      case R(b) => R(GA(b))
    }
    val T = new Functor[F] {
      def map[A, B](fa: FA.F[A] :+: GA.F[A])(f: A => B) = fa match {
        case L(a) => L(FA.T.map(a)(f))
        case R(b) => R(GA.T.map(b)(f))
      }
    }
  }
}

trait Generic[A] {
  type Rep
  def to(r: Rep): A
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
          param.asTerm.name -> param.info.substituteTypes(c.typeParams, t.typeArgs.map(appliedType(typeOf[V[_]], _)))
      else
        Nil
    }
    def rep(t: Type): Type =
      ctors(t.typeSymbol).map(c => params(c, t).map(_._2) match {
        case Nil => typeOf[U]
        case t :: ts => ts.foldLeft(t)((a, b) => appliedType(typeOf[:*:[_, _]], a, b))
      }) match {
        case Nil => t
        case t :: ts => ts.foldLeft(t)((a, b) => appliedType(typeOf[:+:[_, _]], a, b))
      }
    def exprs(t: Type): List[(Tree, Tree)] =
      ctors(t.typeSymbol).map(c =>
        if (c.isModuleClass)
          q"${c.module}"
        else
          q"${c.companion}(..${params(c, t).map(_._1)})"
      ).zip(ctors(t.typeSymbol).map(c => params(c, t).map {
        case (term, tpe) if tpe <:< typeOf[V[_]] => q"V($term)"
        case (term, _) => q"$term"
      } match {
        case Nil => q"U"
        case t :: ts => ts.foldLeft(q"$t": Tree)((a, b) => q":*:($a, $b)")
      }) match {
        case Nil => Nil
        case t :: ts => ts.foldLeft(Vector(t))((a, b) => a.map(t => q"L($t)") :+ q"R($b)")
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
