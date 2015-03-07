package kits

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)
  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B = traverse[({ type F[A] = Const[B, A] })#F, A, B](fa)(f)
}
