package kits

import scala.language.implicitConversions

trait Traverse[F[_]] extends Functor[F] { F =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse(fa)(a => f(a): Identity[B])

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B = {
    type F[A] = B
    traverse(fa)(a => f(a): F[A])
  }

  def fold[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type H[A] = F[G[A]] })#H] =
    new Traverse[({ type H[A] = F[G[A]] })#H] {
      def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        F.traverse(fga)(G.traverse(_)(f))
    }

  class TraverseOps[A](self: F[A]) extends FunctorOps(self) {

    def traverse[B](f: A => B)(implicit B: Unify[Applicative, B]): B.F[F[B.A]] = F.traverse(self)(a => B(f(a)))(B.TC)

    def sequence(implicit A: Unify[Applicative, A]): A.F[F[A.A]] = F.traverse(self)(a => A(a))(A.TC)

    def foldMap[B: Monoid](f: A => B): B = F.foldMap(self)(f)

    def fold(implicit A: Monoid[A]): A = F.fold(self)

  }

}

object Traverse {

  implicit def Ops[A](self: A)(implicit A: Unify[Traverse, A]): Traverse[A.F]#TraverseOps[A.A] = new A.TC.TraverseOps[A.A](A(self))

}
