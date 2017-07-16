package kits

trait Traverse[F[_]] extends Functor[F] { F =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse(fa)(a => f(a): Identity[B])

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B = {
    type G[A] = B
    traverse(fa)(a => f(a): G[B])(B.applicative)
  }

  def fold[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type H[A] = F[G[A]] })#H] =
    new Traverse[({ type H[A] = F[G[A]] })#H] {
      def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        F.traverse(fga)(G.traverse(_)(f))
    }

}

object Traverse extends TraverseFunctions[Traverse]

trait TraverseFunctions[T[F[_]] <: Traverse[F]] extends FunctorFunctions[T] {

  def traverse[F[_], G[_]: Applicative, A, B](fa: F[A])(f: A => G[B])(implicit F: T[F]): G[F[B]] = F.traverse(fa)(f)

  def sequence[F[_], G[_]: Applicative, A](fga: F[G[A]])(implicit F: T[F]): G[F[A]] = F.sequence(fga)

  def foldMap[F[_], A, B: Monoid](fa: F[A])(f: A => B)(implicit F: T[F]): B = F.foldMap(fa)(f)

  def fold[F[_], A: Monoid](fa: F[A])(implicit F: T[F]): A = F.fold(fa)

}
