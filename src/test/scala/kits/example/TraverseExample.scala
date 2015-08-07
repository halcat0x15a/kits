package kits

package example

import org.scalatest.FunSuite

class TraverseExample extends FunSuite {

  type Identity[A] = A

  trait Traverse[F[_]] extends Functor[F] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Identity, A, B](fa)(f)
  }

  implicit val identityApplicative: Applicative[Identity] =
    new Applicative[Identity] {
      def pure[A](a: A): Identity[A] = a
      def ap[A, B](fa: Identity[A])(f: Identity[A => B]): Identity[B] = f(fa)
    }

  implicit val listTraverse: Traverse[List] =
    new Traverse[List] {
      def traverse[F[_], A, B](fa: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
        fa.foldRight(F.pure(Nil: List[B]))((a, ga) => F.ap(ga)(F.map(f(a))(b => b :: _)))
    }

  def monoidApplicative[A](A: Monoid[A]): Applicative[({ type F[B] = A })#F] =
    new Applicative[({ type F[B] = A })#F] {
      def pure[B](b: B) = A.empty
      def ap[B, C](fa: A)(f: A) = A.append(f, fa)
    }

  def foldMap[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Traverse[F], B: Monoid[B]): B = F.traverse[({ type G[A] = B })#G, A, B](fa)(f)(monoidApplicative(B))

  def sequence[F[_], G[_], A](fga: F[G[A]])(implicit F: Traverse[F], G: Applicative[G]): G[F[A]] = F.traverse(fga)(identity)

  test("Examples") {
    assert(sequence(List(Option(1), Option(2), Option(3))) == Some(List(1, 2, 3)))
    assert(sequence(List(Some(1), None, Some(3))) == None)
    assert(foldMap(List("hello", "world"))(identity) == "helloworld")
    assert(foldMap(List("hello", "world"))(x => Vector(x)) == Vector("hello", "world"))
  }

}
