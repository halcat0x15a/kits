package kits

import scala.concurrent.{Future, ExecutionContext}

package object std {
  implicit def sum[A](implicit A: Numeric[A]) = new Monoid[A] {
    def zero: A = A.zero
    def append(x: A, y: A): A = A.plus(x, y)
  }
  implicit def product[A](implicit A: Numeric[A]) = new Monoid[A] {
    def zero: A = A.one
    def append(x: A, y: A): A = A.times(x, y)
  }
  implicit val all = new Monoid[Boolean] {
    def zero: Boolean = true
    def append(x: Boolean, y: Boolean): Boolean = x && y
  }
  implicit val any = new Monoid[Boolean] {
    def zero: Boolean = false
    def append(x: Boolean, y: Boolean): Boolean = x || y
  }
  implicit val string = new Monoid[String] {
    def zero: String = ""
    def append(x: String, y: String): String = x + y
  }
  implicit val unit = new Monoid[Unit] {
    def zero: Unit = ()
    def append(x: Unit, y: Unit): Unit = ()
  }
  implicit val list = new MonadicPlus[List] with Applicative[List] with Traverse[List] {
    def empty[A]: List[A] = Nil
    def plus[A](x: List[A], y: List[A]): List[A] = x ::: y
    def pure[A](a: A): List[A] = a :: Nil
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    def filter[A](fa: List[A])(f: A => Boolean): List[A] = fa.filter(f)
    def traverse[F[_], A, B](fa: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
      fa.foldRight(F.pure(empty[B]))((a, ga) => F(ga)(F.map(f(a))(b => b :: _)))
  }
  implicit val vector = new MonadicPlus[Vector] with Applicative[Vector] with Traverse[Vector] {
    def empty[A]: Vector[A] = Vector.empty
    def plus[A](x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
    def pure[A](a: A): Vector[A] = Vector(a)
    override def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
    def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)
    def filter[A](fa: Vector[A])(f: A => Boolean): Vector[A] = fa.filter(f)
    def traverse[F[_], A, B](fa: Vector[A])(f: A => F[B])(implicit F: Applicative[F]): F[Vector[B]] =
      fa.foldLeft(F.pure(empty[B]))((ga, a) => F(ga)(F.map(f(a))(b => _ :+ b)))
  }
  implicit val option = new MonadicPlus[Option] with Applicative[Option] with Traverse[Option] {
    def empty[A]: Option[A] = None
    def plus[A](x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
    def pure[A](a: A): Option[A] = Some(a)
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    def filter[A](fa: Option[A])(f: A => Boolean): Option[A] = fa.filter(f)
    def traverse[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]): F[Option[B]] =
      fa.fold(F.pure(empty[B]))(a => F.map(f(a))(pure))
    implicit def monoid[A](implicit A: Monoid[A]) = new Monoid[Option[A]] {
      def zero: Option[A] = None
      def append(x: Option[A], y: Option[A]) =
        (x, y) match {
          case (None, None) => None
          case (_, None) => x
          case (None, _) => y
          case (Some(a), Some(b)) => Some(A.append(a, b))
        }
    }
  }
  implicit def either[A] = new Monadic[({ type F[B] = Either[A, B] })#F] with Applicative[({ type F[B] = Either[A, B] })#F] with Traverse[({ type F[B] = Either[A, B] })#F] {
    def pure[B](b: B): Either[A, B] = Right(b)
    override def map[B, C](fa: Either[A, B])(f: B => C): Either[A, C] = fa.right.map(f)
    def flatMap[B, C](fa: Either[A, B])(f: B => Either[A, C]): Either[A, C] = fa.right.flatMap(f)
    def traverse[F[_], B, C](fa: Either[A, B])(f: B => F[C])(implicit F: Applicative[F]): F[Either[A, C]] =
      fa.fold(a => F.pure(Left(a)), b => F.map(f(b))(pure))
  }
  implicit def map[K] = new MonadicPlus[({ type F[A] = Map[K, A] })#F] with Traverse[({ type F[A] = Map[K, A] })#F] {
    def empty[A]: Map[K, A] = Map.empty
    def plus[A](x: Map[K, A], y: Map[K, A]): Map[K, A] = x ++ y
    override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.map { case (k, a) => k -> f(a) }
    def flatMap[A, B](fa: Map[K, A])(f: A => Map[K, B]): Map[K, B] = fa.flatMap { case (_, a) => f(a) }
    def filter[A](fa: Map[K, A])(f: A => Boolean): Map[K, A] = fa.filter { case (_, a) => f(a) }
    def traverse[F[_], A, B](fa: Map[K, A])(f: A => F[B])(implicit F: Applicative[F]): F[Map[K, B]] =
      fa.foldLeft(F.pure(Map.empty[K, B])) { case (ga, (k, a)) => F(ga)(F.map(f(a))(b => _ + (k -> b))) }
    implicit def monoid[K, V](implicit V: Monoid[V]) = new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map.empty
      def append(x: Map[K, V], y: Map[K, V]): Map[K, V] =
        x.foldLeft(y) {
          case (a, (k, v)) => a.updated(k, a.get(k).fold(v)(V.append(_, v)))
        }
    }
  }
  implicit def future(implicit executor: ExecutionContext) = new Monadic[Future] with Applicative[Future] {
    def pure[A](a: A): Future[A] = Future.successful(a)
    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
    def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
  }
  implicit def endo[A] = new Monoid[A => A] {
    def zero: A => A = identity
    def append(f: A => A, g: A => A): A => A = f.andThen(g)
  }
}
