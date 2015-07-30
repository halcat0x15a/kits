package kits

import scala.util.control.TailCalls._

trait Traverse[F[_]] extends Functor[F] { F =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Identity, A, B](fa)(f)

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B = B.applicative.traverse(fa)(f)(this)

  def fold[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)

  def mapAccumL[S, A, B](s: S, fa: F[A])(f: (S, A) => (S, B)): (S, F[B]) =
    Functor.state[TailRec, S].traverse(fa)(a => Monad.State(s => done(f(s, a))))(this).value(s).result

  def mapAccumR[S, A, B](s: S, fa: F[A])(f: (S, A) => (S, B)): (S, F[B]) =
    Functor.state[TailRec, S].dual.traverse(fa)(a => Monad.State(s => done(f(s, a))))(this).value(s).result

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type H[A] = F[G[A]] })#H] =
    new Traverse[({ type H[A] = F[G[A]] })#H] {
      def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        F.traverse(fga)(G.traverse(_)(f))
    }

}

object Traverse {

  def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F

  def traverse[F[_], G[_]: Applicative, A, B](fa: F[A])(f: A => G[B])(implicit F: Traverse[F]): G[F[B]] = F.traverse(fa)(f)

  def sequence[F[_]: Traverse, G[_]: Applicative, A](fga: F[G[A]])(implicit F: Traverse[F]): G[F[A]] = F.sequence(fga)

  def foldMap[F[_], A, B: Monoid](fa: F[A])(f: A => B)(implicit F: Traverse[F]): B = F.foldMap(fa)(f)

  def fold[F[_], A: Monoid](fa: F[A])(implicit F: Traverse[F]): A = F.fold(fa)

  def mapAccumL[F[_], S, A, B](s: S, fa: F[A])(f: (S, A) => (S, B))(implicit F: Traverse[F]): (S, F[B]) = F.mapAccumL(s, fa)(f)

  def mapAccumR[F[_]: Traverse, S, A, B](s: S, fa: F[A])(f: (S, A) => (S, B))(implicit F: Traverse[F]): (S, F[B]) = F.mapAccumR(s, fa)(f)

}
