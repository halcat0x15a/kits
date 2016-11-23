package kits

package std

import scala.concurrent.{Future, ExecutionContext}

trait FutureMonad extends Monad[Future] {

  implicit def executor: ExecutionContext

  override final def pure[A](a: A): Future[A] = Future.successful(a)

  override final def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)

  override final def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)

}
