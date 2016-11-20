package kits

import scala.collection.immutable.IndexedSeq

trait MonadPlus[F[_]] extends Monad[F] { F =>

  def zero[A]: F[A]

  def plus[A](x: F[A], y: F[A]): F[A]

  def filter[A](fa: F[A])(p: A => Boolean): F[A] = flatMap(fa)(a => if (p(a)) pure(a) else zero)

  def monoid[A]: Monoid[F[A]] =
    new Monoid[F[A]] {
      val empty: F[A] = F.zero[A]
      def append(x: F[A], y: F[A]): F[A] = F.plus(x, y)
    }

}

object MonadPlus {

  def plus[F[_], A](x: F[A], y: F[A])(implicit F: MonadPlus[F]): F[A] = F.plus(x, y)

  def filter[F[_], A](fa: F[A])(p: A => Boolean)(implicit F: MonadPlus[F]): F[A] = F.filter(fa)(p)

  trait OptionMonadPlus extends MonadPlus[Option] { self: Functor.OptionFunctor =>
    override final def zero[A]: Option[A] = None
    override final def pure[A](a: A): Option[A] = Some(a)
    override final def plus[A](x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
    override final def filter[A](fa: Option[A])(p: A => Boolean): Option[A] = fa.filter(p)
    override final def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  trait ListMonadPlus extends MonadPlus[List] { self: Functor.ListFunctor =>
    override final def zero[A]: List[A] = Nil
    override final def pure[A](a: A): List[A] = List(a)
    override final def plus[A](x: List[A], y: List[A]): List[A] = x ::: y
    override final def filter[A](fa: List[A])(p: A => Boolean): List[A] = fa.filter(p)
    override final def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  trait VectorMonadPlus extends MonadPlus[Vector] { self: Functor.VectorFunctor =>
    override final def zero[A]: Vector[A] = Vector.empty
    override final def pure[A](a: A): Vector[A] = Vector(a)
    override final def plus[A](x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
    override final def filter[A](fa: Vector[A])(p: A => Boolean): Vector[A] = fa.filter(p)
    override final def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)
  }

  trait IndexedSeqMonadPlus extends MonadPlus[IndexedSeq] { self: Functor.IndexedSeqFunctor =>
    override final def zero[A]: IndexedSeq[A] = IndexedSeq.empty
    override final def pure[A](a: A): IndexedSeq[A] = IndexedSeq(a)
    override final def plus[A](x: IndexedSeq[A], y: IndexedSeq[A]): IndexedSeq[A] = x ++ y
    override final def filter[A](fa: IndexedSeq[A])(p: A => Boolean): IndexedSeq[A] = fa.filter(p)
    override final def flatMap[A, B](fa: IndexedSeq[A])(f: A => IndexedSeq[B]): IndexedSeq[B] = fa.flatMap(f)
  }

  trait StreamMonadPlus extends MonadPlus[Stream] { self: Functor.StreamFunctor =>
    override final def zero[A]: Stream[A] = Stream.empty
    override final def pure[A](a: A): Stream[A] = Stream(a)
    override final def plus[A](x: Stream[A], y: Stream[A]): Stream[A] = x #::: y
    override final def filter[A](fa: Stream[A])(p: A => Boolean): Stream[A] = fa.filter(p)
    override final def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] = fa.flatMap(f)
  }

  trait SetMonadPlus extends MonadPlus[Set] { self: Functor.SetFunctor =>
    override final def zero[A]: Set[A] = Set.empty
    override final def pure[A](a: A): Set[A] = Set(a)
    override final def plus[A](x: Set[A], y: Set[A]): Set[A] = x ++ y
    override final def filter[A](fa: Set[A])(p: A => Boolean): Set[A] = fa.filter(p)
    override final def flatMap[A, B](fa: Set[A])(f: A => Set[B]): Set[B] = fa.flatMap(f)
  }

}
