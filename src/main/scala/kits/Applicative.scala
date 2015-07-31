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
      def ap[A, B](fga: F[G[A]])(f: F[G[A => B]]): F[G[B]] = F.ap(fga)(F.map(f)(g => G.ap(_)(g)))
    }

  def dual: Applicative[F] =
    new Applicative[F] {
      def pure[A](a: A): F[A] = F.pure(a)
      def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = F.ap(f)(F.map(fa)(a => f => f(a)))
    }

}

object Applicative {

  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  def ap[F[_], A, B](fa: F[A])(f: F[A => B])(implicit F: Applicative[F]): F[B] = F.ap(fa)(f)

  def map[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)

  def map[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] = F.map(fa, fb)(f)

  def map[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] = F.map(fa, fb, fc)(f)

  def map[F[_, _], A, B, X](fa: F[X, A])(f: A => B)(implicit F: Functor[({ type G[A] = F[X, A] })#G], dummy: DummyImplicit): F[X, B] = F.map(fa)(f)

  def map[F[_, _], A, B, C, X](fa: F[X, A], fb: F[X, B])(f: (A, B) => C)(implicit F: Applicative[({ type G[A] = F[X, A] })#G], dummy: DummyImplicit): F[X, C] = F.map(fa, fb)(f)

  def map[F[_, _], A, B, C, D, X](fa: F[X, A], fb: F[X, B], fc: F[X, C])(f: (A, B, C) => D)(implicit F: Applicative[({ type G[A] = F[X, A] })#G], dummy: DummyImplicit): F[X, D] = F.map(fa, fb, fc)(f)

  implicit def validation[E](implicit E: Monoid[E]): Applicative[({ type F[A] = Validation[E, A] })#F] =
    new Applicative[({ type F[A] = Validation[E, A] })#F] {
      def pure[A](a: A): Validation[E, A] = Validation(Right(a))
      override def map[A, B](fa: Validation[E, A])(f: A => B): Validation[E, B] = fa.map(f)
      def ap[A, B](fa: Validation[E, A])(f: Validation[E, A => B]): Validation[E, B] = fa.ap(f)
    }

  implicit def reader[F[_], R](implicit F: Applicative[F]): Applicative[({ type G[A] = Monad.Reader[F, R, A] })#G] =
    new Applicative[({ type G[A] = Monad.Reader[F, R, A] })#G] {
      def pure[A](a: A): Monad.Reader[F, R, A] = Monad.Reader(_ => F.pure(a))
      override def map[A, B](fa: Monad.Reader[F, R, A])(f: A => B): Monad.Reader[F, R, B] = fa.map(f)
      def ap[A, B](fa: Monad.Reader[F, R, A])(f: Monad.Reader[F, R, A => B]): Monad.Reader[F, R, B] = fa.ap(f)
    }

  implicit def writer[F[_], W](implicit F: Applicative[F], W: Monoid[W]): Applicative[({ type G[A] = Monad.Writer[F, W, A] })#G] =
    new Applicative[({ type G[A] = Monad.Writer[F, W, A] })#G] {
      def pure[A](a: A): Monad.Writer[F, W, A] = Monad.Writer(F.pure((W.empty, a)))
      override def map[A, B](fa: Monad.Writer[F, W, A])(f: A => B): Monad.Writer[F, W, B] = fa.map(f)
      def ap[A, B](fa: Monad.Writer[F, W, A])(f: Monad.Writer[F, W, A => B]): Monad.Writer[F, W, B] = fa.ap(f)
    }

  implicit def state[F[_], S](implicit F: Monad[F]): Monad[({ type G[A] = Monad.State[F, S, A] })#G] =
    new Monad[({ type G[A] = Monad.State[F, S, A] })#G] {
      def pure[A](a: A): Monad.State[F, S, A] = Monad.State(s => F.pure((s, a)))
      override def map[A, B](fa: Monad.State[F, S, A])(f: A => B): Monad.State[F, S, B] = fa.map(f)
      def flatMap[A, B](fa: Monad.State[F, S, A])(f: A => Monad.State[F, S, B]): Monad.State[F, S, B] = fa.flatMap(f)
    }

  case class Validation[E, A](value: Either[E, A]) extends AnyVal {

    def map[B](f: A => B): Validation[E, B] = Validation(value.right.map(f))

    def ap[B](f: Validation[E, A => B])(implicit E: Monoid[E]): Validation[E, B] =
      (f.value, value) match {
        case (Right(f), Right(a)) => Validation(Right(f(a)))
        case (Right(_), Left(e)) => Validation(Left(e))
        case (Left(e), Right(_)) => Validation(Left(e))
        case (Left(x), Left(y)) => Validation(Left(E.append(x, y)))
      }

    def traverse[F[_], B](f: A => F[B])(implicit F: Applicative[F]): F[Validation[E, B]] =
      value match {
        case Left(e) => F.pure(Validation(Left(e)))
        case Right(a) => F.map(f(a))(b => Validation(Right(b)))
      }

  }

}
