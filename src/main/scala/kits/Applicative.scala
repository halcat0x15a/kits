package kits

trait Applicative[F[_]] extends Functor[F] { F =>

  def pure[A](a: A): F[A]

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))

  def map[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ap(fb)(map(fa)(a => f(a, _)))

  def map[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = ap(fc)(map(fa, fb)((a, b) => f(a, b, _)))

  def traverse[G[_], A, B](ga: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] = G.traverse(ga)(f)(this)

  def compose[G[_]](implicit G: Applicative[G]): Applicative[({ type H[A] = F[G[A]] })#H] =
    new Applicative[({ type H[A] = F[G[A]] })#H] {
      def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
      def ap[A, B](fga: F[G[A]])(f: F[G[A => B]]): F[G[B]] = F.map(fga, f)(G.ap(_)(_))
    }

  lazy val dual: Applicative[F] =
    new Applicative[F] {
      def pure[A](a: A): F[A] = F.pure(a)
      def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = F.map(f, fa)(_(_))
    }

}

object Applicative {

  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  def ap[F[_], A, B](fa: F[A])(f: F[A => B])(implicit F: Applicative[F]): F[B] = F.ap(fa)(f)

  def map[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)

  def map[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] = F.map(fa, fb)(f)

  def map[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] = F.map(fa, fb, fc)(f)

  implicit def validation[E: Monoid]: Applicative[({ type F[A] = Validation[E, A] })#F] =
    new Applicative[({ type F[A] = Validation[E, A] })#F] {
      def pure[A](a: A): Validation[E, A] = Validation(Right(a))
      override def map[A, B](fa: Validation[E, A])(f: A => B): Validation[E, B] = fa.map(f)
      def ap[A, B](fa: Validation[E, A])(f: Validation[E, A => B]): Validation[E, B] = fa.ap(f)
    }

  implicit def reader[F[_], R](implicit F: Applicative[F]): Applicative[({ type G[A] = Reader[F, R, A] })#G] =
    new Applicative[({ type G[A] = Reader[F, R, A] })#G] {
      def pure[A](a: A): Reader[F, R, A] = Reader(_ => F.pure(a))
      override def map[A, B](fa: Reader[F, R, A])(f: A => B): Reader[F, R, B] = fa.map(f)
      def ap[A, B](fa: Reader[F, R, A])(f: Reader[F, R, A => B]): Reader[F, R, B] = fa.ap(f)
    }

  implicit def writer[F[_], W](implicit F: Applicative[F], W: Monoid[W]): Applicative[({ type G[A] = Writer[F, W, A] })#G] =
    new Applicative[({ type G[A] = Writer[F, W, A] })#G] {
      def pure[A](a: A): Writer[F, W, A] = Writer(F.pure((W.empty, a)))
      override def map[A, B](fa: Writer[F, W, A])(f: A => B): Writer[F, W, B] = fa.map(f)
      def ap[A, B](fa: Writer[F, W, A])(f: Writer[F, W, A => B]): Writer[F, W, B] = fa.ap(f)
    }

  implicit def state[F[_], S](implicit F: Monad[F]): Monad[({ type G[A] = State[F, S, A] })#G] =
    new Monad[({ type G[A] = State[F, S, A] })#G] {
      def pure[A](a: A): State[F, S, A] = State(s => F.pure((s, a)))
      override def map[A, B](fa: State[F, S, A])(f: A => B): State[F, S, B] = fa.map(f)
      def flatMap[A, B](fa: State[F, S, A])(f: A => State[F, S, B]): State[F, S, B] = fa.flatMap(f)
    }

}
