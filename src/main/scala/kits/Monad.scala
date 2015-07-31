package kits

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

}

object Monad {

  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  implicit def reader[F[_], R](implicit F: Monad[F]): Monad[({ type G[A] = Reader[F, R, A] })#G] =
    new Monad[({ type G[A] = Reader[F, R, A] })#G] {
      def pure[A](a: A): Reader[F, R, A] = Reader(_ => F.pure(a))
      def flatMap[A, B](fa: Reader[F, R, A])(f: A => Reader[F, R, B]): Reader[F, R, B] = fa.flatMap(f)
    }

  implicit def writer[F[_], W](implicit F: Monad[F], W: Monoid[W]): Monad[({ type G[A] = Writer[F, W, A] })#G] =
    new Monad[({ type G[A] = Writer[F, W, A] })#G] {
      def pure[A](a: A): Writer[F, W, A] = Writer(F.pure((W.empty, a)))
      override def map[A, B](fa: Writer[F, W, A])(f: A => B): Writer[F, W, B] = fa.map(f)
      def flatMap[A, B](fa: Writer[F, W, A])(f: A => Writer[F, W, B]): Writer[F, W, B] = fa.flatMap(f)
    }

  case class Reader[F[_], R, A](value: R => F[A]) extends AnyVal {

    def map[B](f: A => B)(implicit F: Functor[F]): Reader[F, R, B] =
      Reader(r => F.map(value(r))(f))

    def ap[B](f: Reader[F, R, A => B])(implicit F: Applicative[F]): Reader[F, R, B] =
      Reader(r => F.ap(value(r))(f.value(r)))

    def flatMap[B](f: A => Reader[F, R, B])(implicit F: Monad[F]): Reader[F, R, B] =
      Reader(r => F.flatMap(value(r))(a => f(a).value(r)))

  }

  case class Writer[F[_], W, A](value: F[(W, A)]) extends AnyVal {

    def map[B](f: A => B)(implicit F: Functor[F]): Writer[F, W, B] =
      Writer(F.map(value) { case (w, a) => (w, f(a)) })

    def ap[B](f: Writer[F, W, A => B])(implicit F: Applicative[F], W: Monoid[W]): Writer[F, W, B] =
      Writer(F.map(f.value, value) { case ((x, f), (y, a)) => (W.append(x, y), f(a)) })

    def flatMap[B](f: A => Writer[F, W, B])(implicit F: Monad[F], W: Monoid[W]): Writer[F, W, B] =
      Writer(F.flatMap(value) { case (x, a) => F.map(f(a).value) { case (y, b) => (W.append(x, y), b) } })

    def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[Writer[F, W, B]] =
      G.map(F.traverse(value) { case (w, a) => G.map(f(a))(b => (w, b)) })(Writer(_))

  }

  case class State[F[_], S, A](value: S => F[(S, A)]) extends AnyVal {

    def map[B](f: A => B)(implicit F: Functor[F]): State[F, S, B] =
      State(s => F.map(value(s)) { case (s, a) => (s, f(a)) })

    def flatMap[B](f: A => State[F, S, B])(implicit F: Monad[F]): State[F, S, B] =
      State(s => F.flatMap(value(s)) { case (s, a) => f(a).value(s) })

  }

}
