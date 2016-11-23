package kits

package std

import scala.collection.immutable.IndexedSeq

trait IndexedSeqFunctor extends Functor[IndexedSeq] {

  override final def map[A, B](fa: IndexedSeq[A])(f: A => B): IndexedSeq[B] = fa.map(f)

}

trait IndexedSeqMonadPlus extends MonadPlus[IndexedSeq] { self: IndexedSeqFunctor =>

  override final def zero[A]: IndexedSeq[A] = IndexedSeq.empty

  override final def pure[A](a: A): IndexedSeq[A] = IndexedSeq(a)

  override final def plus[A](x: IndexedSeq[A], y: IndexedSeq[A]): IndexedSeq[A] = x ++ y

  override final def filter[A](fa: IndexedSeq[A])(p: A => Boolean): IndexedSeq[A] = fa.filter(p)

  override final def flatMap[A, B](fa: IndexedSeq[A])(f: A => IndexedSeq[B]): IndexedSeq[B] = fa.flatMap(f)

  override final def flatten[A](ffa: IndexedSeq[IndexedSeq[A]]): IndexedSeq[A] = ffa.flatten

}

trait IndexedSeqTraverse extends Traverse[IndexedSeq] { self: IndexedSeqFunctor =>

  override final def traverse[F[_], A, B](fa: IndexedSeq[A])(f: A => F[B])(implicit F: Applicative[F]): F[IndexedSeq[B]] =
    fa.foldLeft(F.pure(IndexedSeq.empty[B]))((ga, a) => F.map2(ga, f(a))(_ :+ _))

}
