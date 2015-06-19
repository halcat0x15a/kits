package kits

trait Traverse[F[_]] extends Functor[F] { F =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Identity, A, B](fa)(f)

  def compose[G[_]](implicit G: Traverse[G]) = new Traverse[({ type H[A] = F[G[A]] })#H] {
    def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
      F.traverse(fga)(G.traverse(_)(f))
  }

}

object Traverse {

  def traverse[F[_], G[_], A, B](fa: F[A])(f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[F[B]] = F.traverse(fa)(f)

  def sequence[F[_]: Traverse, G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  def foldMap[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Traverse[F], B: Monoid[B]): B = F.traverse[({ type G[A] = B })#G, A, B](fa)(f)(B.applicative)

  def fold[F[_]: Traverse, A: Monoid](fa: F[A]): A = foldMap(fa)(identity)

  def mapAccumL[F[_]: Traverse, S, A, B](fa: F[A], s: S)(f: (S, A) => (S, B)): (S, F[B]) =
    traverse[F, ({ type G[A] = State[S, A] })#G, A, B](fa)(a => s => f(s, a)).apply(s)

  def mapAccumR[F[_], S, A, B](fa: F[A], s: S)(f: (S, A) => (S, B))(implicit F: Traverse[F]): (S, F[B]) =
    F.traverse[({ type G[A] = State[S, A] })#G, A, B](fa)(a => s => f(s, a))(Functor.state[S].flip)(s)

}
