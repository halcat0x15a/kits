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
      override def map[A, B](fa: Reader[F, R, A])(f: A => B): Reader[F, R, B] = fa.map(f)
      override def ap[A, B](fa: Reader[F, R, A])(f: Reader[F, R, A => B]): Reader[F, R, B] = fa.ap(f)
      def flatMap[A, B](fa: Reader[F, R, A])(f: A => Reader[F, R, B]): Reader[F, R, B] = fa.flatMap(f)
    }

  implicit def writer[F[_], W](implicit F: Monad[F], W: Monoid[W]): Monad[({ type G[A] = Writer[F, W, A] })#G] =
    new Monad[({ type G[A] = Writer[F, W, A] })#G] {
      def pure[A](a: A): Writer[F, W, A] = Writer(F.pure((W.empty, a)))
      override def map[A, B](fa: Writer[F, W, A])(f: A => B): Writer[F, W, B] = fa.map(f)
      override def ap[A, B](fa: Writer[F, W, A])(f: Writer[F, W, A => B]): Writer[F, W, B] = fa.ap(f)
      def flatMap[A, B](fa: Writer[F, W, A])(f: A => Writer[F, W, B]): Writer[F, W, B] = fa.flatMap(f)
    }

}
