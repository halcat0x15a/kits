package kits

package example

import org.scalatest.FunSuite

class ApplicativeExample extends FunSuite {

  test("map2") {
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
    implicit def eitherApplicative[E]: Applicative[({ type F[A] = Either[E, A] })#F] =
      new Applicative[({ type F[A] = Either[E, A] })#F] {
        def pure[A](a: A): Either[E, A] = Right(a)
        def ap[A, B](fa: Either[E, A])(f: Either[E, A => B]): Either[E, B] = f.right.flatMap(f => fa.right.map(a => f(a)))
      }
    def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] =
      F.ap(fb)(F.map(fa)(a => f(a, _)))
    def map3[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] =
      F.ap(fc)(map2(fa, fb)((a, b) => f(a, b, _)))
    implicit val optionMonad: Monad[Option] =
      new Monad[Option] {
        def pure[A](a: A): Option[A] = Some(a)
        def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
      }
    case class User(id: Int, name: String)
    assert(map2(Option(346), Option("halcat"))(User) == Some(User(346, "halcat")))
    assert((for (id <- Option(346); name <- Option("halcat")) yield User(id, name)) == Some(User(346, "halcat")))
  }

  test("map") {
    assert(Applicative.map2(List(1, 2), List(3))(_ + _) == List(4, 5))
    assert(Applicative.map3(Option("foo"), None, Some("bar"))(_ + _ + _) == None)
    val hoge: Either[String, Int] = Left("hoge")
    val fuga: Either[String, Int] = Left("fuga")
    assert(Applicative.map2(hoge, fuga)(_ + _) == Left("hogefuga"))
  }

}
