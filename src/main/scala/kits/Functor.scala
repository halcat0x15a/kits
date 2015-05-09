package kits

import scala.concurrent.{Future, ExecutionContext}

import scala.util.Try

trait Functor[F[_]] extends Any { F =>

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def compose[G[_]](implicit G: Functor[G]) = new Functor[({ type H[A] = F[G[A]] })#H] {

    def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F.map(fga)(G.map(_)(f))

  }

}

object Functor extends FunctorImplicits

private[kits] trait FunctorImplicits extends LowPriorityFunctorImplicits {

  implicit val identity = new Monad[Identity] with Traverse[Identity] {

    def pure[A](a: A): A = a

    override def map[A, B](fa: A)(f: A => B): B = f(fa)

    def flatMap[A, B](fa: A)(f: A => B): B = f(fa)

    def traverse[F[_]: Applicative, A, B](fa: A)(f: A => F[B]): F[B] = f(fa)

  }

  implicit val list = new Monad[List] with Traverse[List] {

    def pure[A](a: A): List[A] = a :: Nil

    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

    def traverse[F[_], A, B](fa: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
      fa.foldRight(F.pure(Nil: List[B]))((a, ga) => F(ga)(F.map(f(a))(b => b :: _)))

  }

  implicit val vector = new Monad[Vector] with Traverse[Vector] {

    def pure[A](a: A): Vector[A] = Vector(a)

    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)

    def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)

    def traverse[F[_], A, B](fa: Vector[A])(f: A => F[B])(implicit F: Applicative[F]): F[Vector[B]] =
      fa.foldLeft(F.pure(Vector.empty[B]))((ga, a) => F(f(a))(F.map(ga)(a => a :+ _)))

  }

  implicit val option = new Monad[Option] with Traverse[Option] {

    def pure[A](a: A): Option[A] = Some(a)

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    def traverse[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]): F[Option[B]] =
      fa.fold(F.pure(None: Option[B]))(a => F.map(f(a))(pure))

  }

  implicit def map[K] = new Traverse[({ type F[A] = Map[K, A] })#F] {

    def traverse[F[_], A, B](fa: Map[K, A])(f: A => F[B])(implicit F: Applicative[F]): F[Map[K, B]] =
      fa.foldLeft(F.pure(Map.empty[K, B])) { case (ga, (k, a)) => F(f(a))(F.map(ga)(a => b => a + (k -> b))) }

  }

  implicit val set = new Monad[Set] with Traverse[Set] {

    def pure[A](a: A): Set[A] = Set(a)

    override def map[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(f)

    def flatMap[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(f)

    def traverse[F[_], A, B](fa: Set[A])(f: A => F[B])(implicit F: Applicative[F]): F[Set[B]] =
      fa.foldLeft(F.pure(Set.empty[B]))((ga, a) => F(f(a))(F.map(ga)(a => a + _)))

  }

  implicit def right[A] = new Monad[({ type F[B] = Either[A, B] })#F] with Traverse[({ type F[B] = Either[A, B] })#F] {

    def pure[B](b: B): Either[A, B] = Right(b)

    override def map[B, C](fb: Either[A, B])(f: B => C): Either[A, C] = fb.right.map(f)

    def flatMap[B, C](fb: Either[A, B])(f: B => Either[A, C]): Either[A, C] = fb.right.flatMap(f)

    def traverse[F[_], B, C](fb: Either[A, B])(f: B => F[C])(implicit F: Applicative[F]): F[Either[A, C]] =
      fb.fold(a => F.pure(Left(a)), b => F.map(f(b))(pure))

  }

  implicit def left[B] = new Monad[({ type F[A] = Either[A, B] })#F] with Traverse[({ type F[A] = Either[A, B] })#F] {

    def pure[A](a: A): Either[A, B] = Left(a)

    override def map[A, C](fa: Either[A, B])(f: A => C): Either[C, B] = fa.left.map(f)

    def flatMap[A, C](fa: Either[A, B])(f: A => Either[C, B]): Either[C, B] = fa.left.flatMap(f)

    def traverse[F[_], A, C](fa: Either[A, B])(f: A => F[C])(implicit F: Applicative[F]): F[Either[C, B]] =
      fa.fold(a => F.map(f(a))(pure), b => F.pure(Right(b)))

  }

  implicit def future(implicit executor: ExecutionContext) = new Monad[Future] {

    def pure[A](a: A): Future[A] = Future.successful(a)

    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

    def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

  }

  implicit val `try` = new Monad[Try] {

    def pure[A](a: A): Try[A] = Try(a)

    override def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)

    def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

  }

}

private[kits] trait LowPriorityFunctorImplicits {

  implicit def functor[F[_], T, R](implicit F: Generic[F[T]] { type Rep = R }, R: Instance[Functor, R]) = new Functor[F] {

    def map[A, B](fa: F[A])(f: A => B): F[B] =
      F.to(R.T.map(R(F.from(fa.asInstanceOf[F[T]])))(a => f(a.asInstanceOf[A])).asInstanceOf[R]).asInstanceOf[F[B]]

  }

  implicit def traverse[F[_], T, R](implicit F: Generic[F[T]] { type Rep = R }, R: Instance[Traverse, R]) = new Traverse[F] {

    def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
      G.map(R.T.traverse(R(F.from(fa.asInstanceOf[F[T]])))(a => f(a.asInstanceOf[A])))(r => F.to(r.asInstanceOf[R]).asInstanceOf[F[B]])

  }

}
