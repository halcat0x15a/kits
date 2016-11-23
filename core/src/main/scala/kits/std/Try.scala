package kits

package std

import scala.util.{Try, Success}

trait TryMonad extends Monad[Try] {

  override final def pure[A](a: A): Try[A] = Success(a)

  override final def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa.map(f)

  override final def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

}
