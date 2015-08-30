package kits

import scala.util.control.TailCalls._

trait Traverse[F[_]] extends Functor[F] { F =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse(fa)(a => Identity(f(a))).value

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B = B.applicative.traverse(fa)(f)(this)

  def fold[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)

  def mapAccumL[S, A, B](s: S, fa: F[A])(f: (S, A) => (S, B)): (S, F[B]) =
    Applicative.state[TailRec, S].traverse(fa)(a => State[TailRec, S, B](s => done(f(s, a))))(this).value(s).result

  def mapAccumR[S, A, B](s: S, fa: F[A])(f: (S, A) => (S, B)): (S, F[B]) =
    Applicative.state[TailRec, S].dual.traverse(fa)(a => State[TailRec, S, B](s => done(f(s, a))))(this).value(s).result

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type H[A] = F[G[A]] })#H] =
    new Traverse[({ type H[A] = F[G[A]] })#H] {
      def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        F.traverse(fga)(G.traverse(_)(f))
    }

}

object Traverse {

  def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F

  def traverse[F[_]: Traverse, A, GB](fa: F[A])(f: A => GB)(implicit GB: Instance[Applicative, GB]): GB.F[F[GB.A]] = GB.T.traverse(fa)(a => GB(f(a)))

  def sequence[F[_]: Traverse, GA](fga: F[GA])(implicit GA: Instance[Applicative, GA]): GA.F[F[GA.A]] = traverse(fga)(identity)

  def foldMap[F[_], A, B: Monoid](fa: F[A])(f: A => B)(implicit F: Traverse[F]): B = F.foldMap(fa)(f)

  def fold[F[_], A: Monoid](fa: F[A])(implicit F: Traverse[F]): A = F.fold(fa)

  def mapAccumL[F[_], S, A, B](s: S, fa: F[A])(f: (S, A) => (S, B))(implicit F: Traverse[F]): (S, F[B]) = F.mapAccumL(s, fa)(f)

  def mapAccumR[F[_], S, A, B](s: S, fa: F[A])(f: (S, A) => (S, B))(implicit F: Traverse[F]): (S, F[B]) = F.mapAccumR(s, fa)(f)

  implicit def writer[F[_]: Traverse, W]: Traverse[({ type G[A] = Writer[F, W, A] })#G] =
    new Traverse[({ type G[A] = Writer[F, W, A] })#G] {
      override def map[A, B](fa: Writer[F, W, A])(f: A => B): Writer[F, W, B] = fa.map(f)
      def traverse[G[_]: Applicative, A, B](fa: Writer[F, W, A])(f: A => G[B]): G[Writer[F, W, B]] = fa.traverse(f)
    }

}
