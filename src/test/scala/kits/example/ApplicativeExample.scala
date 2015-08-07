package kits

package example

import org.scalatest.FunSuite

import Applicative.Validation

class ApplicativeExample extends FunSuite {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
    def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
    override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
    override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))
  }

  implicit val optionMonad: Monad[Option] =
    new Monad[Option] {
      def pure[A](a: A): Option[A] = Some(a)
      def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    }

  def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] =
    F.ap(fb)(F.map(fa)(a => f(a, _)))
  
  def map3[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] =
    F.ap(fc)(map2(fa, fb)((a, b) => f(a, b, _)))

  type Result[A] = Validation[List[String], A]

  def fail[A](s: String): Result[A] = Validation(Left(List(s)))

  def succeed[A](a: A): Result[A] = Validation(Right(a))

  case class User(id: Int, name: String)

  test("map2") {
    assert(map2(Option(346), Option("halcat"))(User) == Some(User(346, "halcat")))
  }

  test("for") {
    assert((for (id <- Option(346); name <- Option("halcat")) yield User(id, name)) == Some(User(346, "halcat")))
  }

  test("map") {
    assert(kits.Applicative.map(List(1, 2), List(3))(_ + _) == List(4, 5))
    assert(kits.Applicative.map(Some("foo"), None, Some("bar"))(_ + _ + _) == None)
    assert(kits.Applicative.map(fail[Int]("foo"), succeed(1), fail[Int]("bar"))(_ + _ + _) == Validation(Left(List("foo", "bar"))))
  }

}
