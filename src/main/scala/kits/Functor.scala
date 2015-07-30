package kits

import scala.concurrent.{Future, ExecutionContext}
import scala.util.Try
import scala.util.control.TailCalls._

import Applicative.Validation
import Monad.{Reader, Writer, State}

trait Functor[F[_]] { F =>

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def compose[G[_]](implicit G: Functor[G]): Functor[({ type H[A] = F[G[A]] })#H] =
    new Functor[({ type H[A] = F[G[A]] })#H] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F.map(fga)(G.map(_)(f))
    }

}

object Functor {

  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  implicit val identity: Monad[Identity] with Traverse[Identity] =
    new Monad[Identity] with Traverse[Identity] {
      def pure[A](a: A): A = a
      override def map[A, B](fa: A)(f: A => B): B = f(fa)
      def flatMap[A, B](fa: A)(f: A => B): B = f(fa)
      def traverse[F[_]: Applicative, A, B](fa: A)(f: A => F[B]): F[B] = f(fa)
    }

  implicit val list: Monad[List] with Traverse[List] =
    new Monad[List] with Traverse[List] {
      def pure[A](a: A): List[A] = a :: Nil
      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
      def traverse[F[_], A, B](fa: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
        fa.foldRight(F.pure(Nil: List[B]))((a, ga) => F.map(f(a), ga)(_ :: _))
    }

  implicit val vector: Monad[Vector] with Traverse[Vector] =
    new Monad[Vector] with Traverse[Vector] {
      def pure[A](a: A): Vector[A] = Vector(a)
      override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
      def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)
      def traverse[F[_], A, B](fa: Vector[A])(f: A => F[B])(implicit F: Applicative[F]): F[Vector[B]] =
        fa.foldLeft(F.pure(Vector.empty[B]))((ga, a) => F.map(ga, f(a))(_ :+ _))
    }

  implicit val option: Monad[Option] with Traverse[Option] =
    new Monad[Option] with Traverse[Option] {
      def pure[A](a: A): Option[A] = Some(a)
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
      def traverse[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]): F[Option[B]] =
        fa.fold(F.pure(None: Option[B]))(a => F.map(f(a))(pure))
    }

  implicit def map[K]: Traverse[({ type F[A] = Map[K, A] })#F] =
    new Traverse[({ type F[A] = Map[K, A] })#F] {
      def traverse[F[_], A, B](fa: Map[K, A])(f: A => F[B])(implicit F: Applicative[F]): F[Map[K, B]] =
        fa.foldLeft(F.pure(Map.empty[K, B])) { case (ga, (k, a)) => F.map(ga, f(a))((a, b) => a + (k -> b)) }
    }

  implicit val set: Monad[Set] with Traverse[Set] =
    new Monad[Set] with Traverse[Set] {
      def pure[A](a: A): Set[A] = Set(a)
      override def map[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(f)
      def flatMap[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(f)
      def traverse[F[_], A, B](fa: Set[A])(f: A => F[B])(implicit F: Applicative[F]): F[Set[B]] =
        fa.foldLeft(F.pure(Set.empty[B]))((ga, a) => F.map(ga, f(a))(_ + _))
    }

  implicit def either[E]: Monad[({ type F[A] = Either[E, A] })#F] with Traverse[({ type F[A] = Either[E, A] })#F] =
    new Monad[({ type F[A] = Either[E, A] })#F] with Traverse[({ type F[A] = Either[E, A] })#F] {
      def pure[A](a: A): Either[E, A] = Right(a)
      override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa.right.map(f)
      def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.right.flatMap(f)
      def traverse[F[_], A, B](fa: Either[E, A])(f: A => F[B])(implicit F: Applicative[F]): F[Either[E, B]] =
        fa.fold(e => F.pure(Left(e)), a => F.map(f(a))(pure))
    }

  implicit def future(implicit executor: ExecutionContext): Monad[Future] =
    new Monad[Future] {
      def pure[A](a: A): Future[A] = Future.successful(a)
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
    }

  implicit val `try`: Monad[Try] =
    new Monad[Try] {
      def pure[A](a: A): Try[A] = Try(a)
      override def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)
      def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
    }

  implicit val tailrec: Monad[TailRec] =
    new Monad[TailRec] {
      def pure[A](a: A): TailRec[A] = done(a)
      override def map[A, B](fa: TailRec[A])(f: A => B): TailRec[B] = fa.map(f)
      def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] = tailcall(fa.flatMap(f))
    }

  implicit def validation[E](implicit E: Monoid[E]): Applicative[({ type F[A] = Validation[E, A] })#F] =
    new Applicative[({ type F[A] = Validation[E, A] })#F] {
      def pure[A](a: A): Validation[E, A] = Validation(Right(a))
      override def map[A, B](fa: Validation[E, A])(f: A => B): Validation[E, B] = Validation(fa.value.right.map(f))
      def ap[A, B](fa: Validation[E, A])(f: Validation[E, A => B]): Validation[E, B] =
        (f.value, fa.value) match {
          case (Right(f), Right(a)) => Validation(Right(f(a)))
          case (Right(_), Left(e)) => Validation(Left(e))
          case (Left(e), Right(_)) => Validation(Left(e))
          case (Left(x), Left(y)) => Validation(Left(E.append(x, y)))
        }
    }

  implicit def reader[F[_], R](implicit F: Monad[F]): Monad[({ type G[A] = Reader[F, R, A] })#G] =
    new Monad[({ type G[A] = Reader[F, R, A] })#G] {
      def pure[A](a: A): Reader[F, R, A] = Reader(_ => F.pure(a))
      def flatMap[A, B](fa: Reader[F, R, A])(f: A => Reader[F, R, B]): Reader[F, R, B] =
        Reader(r => F.flatMap(fa.value(r))(a => f(a).value(r)))
    }

  implicit def writer[F[_], W](implicit F: Monad[F], W: Monoid[W]): Monad[({ type G[A] = Writer[F, W, A] })#G] =
    new Monad[({ type G[A] = Writer[F, W, A] })#G] {
      def pure[A](a: A): Writer[F, W, A] = Writer(F.pure((W.empty, a)))
      def flatMap[A, B](fa: Writer[F, W, A])(f: A => Writer[F, W, B]): Writer[F, W, B] =
        Writer(F.flatMap(fa.value) { case (x, a) => F.map(f(a).value) { case (y, b) => (W.append(x, y), b) } })
    }

  implicit def state[F[_], S](implicit F: Monad[F]): Monad[({ type G[A] = State[F, S, A] })#G] =
    new Monad[({ type G[A] = State[F, S, A] })#G] {
      def pure[A](a: A): State[F, S, A] = State(s => F.pure((s, a)))
      def flatMap[A, B](fa: State[F, S, A])(f: A => State[F, S, B]): State[F, S, B] =
        State(s => F.flatMap(fa.value(s)) { case (s, a) => f(a).value(s) })
    }

}
