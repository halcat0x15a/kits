package kits

package std

import scala.util.control.TailCalls._

trait TailRecMonad extends Monad[TailRec] {

  override final def pure[A](a: A): TailRec[A] = done(a)

  override final def map[A, B](fa: TailRec[A])(f: A => B): TailRec[B] = fa.map(f)

  override final def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] = tailcall(fa.flatMap(f))

}
