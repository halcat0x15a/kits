package kits

import scala.language.experimental.macros

sealed trait U

case object U extends U {

  implicit val monoid = new Monoid[U] {

    def empty: U = U

    def append(x: U, y: U): U = U

  }

  implicit val traverse = new Traverse[({ type F[_] = U })#F] {

    override def map[A, B](fa: U)(f: A => B): U = U

    def traverse[F[_], A, B](fa: U)(f: A => F[B])(implicit F: Applicative[F]): F[U] = F.pure(U)

  }

}

case class Par[A](a: A)

object Par {

  implicit def monoid[A](implicit A: Monoid[A]) = new Monoid[Par[A]] {

    def empty: Par[A] = Par(A.empty)

    def append(x: Par[A], y: Par[A]): Par[A] = (x, y) match {
      case (Par(x), Par(y)) => Par(A.append(x, y))
    }

  }

  implicit val traverse = new Traverse[Par] {

    override def map[A, B](fa: Par[A])(f: A => B): Par[B] = fa match {
      case Par(a) => Par(f(a))
    }

    def traverse[F[_], A, B](fa: Par[A])(f: A => F[B])(implicit F: Applicative[F]): F[Par[B]] = F.map(f(fa.a))(Par.apply)

  }

}

case class :*:[A, B](a: A, b: B)

object :*: {

  implicit def monoid[A, B](implicit A: Monoid[A], B: Monoid[B]) = new Monoid[A :*: B] {

    def empty: A :*: B = :*:(A.empty, B.empty)

    def append(x: A :*: B, y: A :*: B): A :*: B = (x, y) match {
      case (ax :*: bx, ay :*: by) => :*:(A.append(ax, ay), B.append(bx, by))
    }

  }

  implicit def functor[LA0, RA0, A0](implicit LA0: Unapply[Functor, LA0] { type A = A0 }, RA0: Unapply[Functor, RA0] { type A = A0 }) = new ProductUnapply[Functor, LA0, RA0] {

    type A = A0

    val LA = LA0

    val RA = RA0

    val T = new Functor[F] {

      def map[A, B](fa: F[A])(f: A => B): F[B] = fa match {
        case a :*: b => :*:(LA.T.map(a)(f), RA.T.map(b)(f))
      }

    }

  }

  implicit def traverse[LA0, RA0, A0](implicit LA0: Unapply[Traverse, LA0] { type A = A0 }, RA0: Unapply[Traverse, RA0] { type A = A0 }) = new ProductUnapply[Traverse, LA0, RA0] {

    type A = A0

    val LA = LA0

    val RA = RA0

    val T = new Traverse[F] {

      def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = fa match {
        case a :*: b => G(RA.T.traverse(b)(f))(G.map(LA.T.traverse(a)(f))(a => :*:(a, _)))
      }

    }

  }

  trait ProductUnapply[T[_[_]], LA, RA] extends Unapply[T, LA :*: RA] { self =>

    type F[A] = LA.F[A] :*: RA.F[A]

    val LA: Unapply[T, LA] { type A = self.A }

    val RA: Unapply[T, RA] { type A = self.A }

    def apply(fa: LA :*: RA): F[A] = fa match {
      case a :*: b => :*:(LA(a), RA(b))
    }

  }

}

sealed trait :+:[+A, +B]

case class L[A](a: A) extends (A :+: Nothing)

case class R[B](b: B) extends (Nothing :+: B)

object :+: {

  implicit def functor[LA0, RA0, A0](implicit LA0: Unapply[Functor, LA0] { type A = A0 }, RA0: Unapply[Functor, RA0] { type A = A0 }) = new SumUnapply[Functor, LA0, RA0] {

    type A = A0

    val LA = LA0

    val RA = RA0

    val T = new Functor[F] {

      def map[A, B](fa: F[A])(f: A => B): F[B] = fa match {
        case L(a) => L(LA.T.map(a)(f))
        case R(b) => R(RA.T.map(b)(f))
      }

    }

  }
  
  implicit def traverse[LA0, RA0, A0](implicit LA0: Unapply[Traverse, LA0] { type A = A0 }, RA0: Unapply[Traverse, RA0] { type A = A0 }) = new SumUnapply[Traverse, LA0, RA0] {

    type A = A0

    val LA = LA0

    val RA = RA0

    val T = new Traverse[F] {

      def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = fa match {
        case L(a) => G.map(LA.T.traverse(a)(f))(L.apply)
        case R(b) => G.map(RA.T.traverse(b)(f))(R.apply)
      }
      
    }

  }

  trait SumUnapply[T[_[_]], LA, RA] extends Unapply[T, LA :+: RA] { self =>

    type F[A] = LA.F[A] :+: RA.F[A]

    val LA: Unapply[T, LA] { type A = self.A }

    val RA: Unapply[T, RA] { type A = self.A }

    def apply(fa: LA :+: RA): F[A] = fa match {
      case L(a) => L(LA(a))
      case R(b) => R(RA(b))
    }

  }

}

trait Generic[A] extends Any {

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
          param.asTerm.name -> param.info.substituteTypes(c.typeParams, t.typeArgs.map(appliedType(typeOf[Par[_]], _)))
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
        case (term, tpe) if tpe <:< typeOf[Par[_]] => q"Par($term)"
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
