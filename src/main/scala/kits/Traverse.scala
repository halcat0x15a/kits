package kits

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)
}

object Traverse {
  def sequence[F[_], G[_], A](fga: F[G[A]])(implicit F: Traverse[F], G: Applicative[G]): G[F[A]] = F.traverse(fga)(identity)
  def foldMap[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Traverse[F], B: Monoid[B]): B = F.traverse[({ type F[A] = Const[B, A] })#F, A, B](fa)(f)
  def fold[F[_]: Traverse, A: Monoid](fa: F[A]): A = foldMap(fa)(identity)
}
