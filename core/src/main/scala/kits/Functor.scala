package kits

import scala.language.implicitConversions

import scala.collection.immutable.IndexedSeq
import scala.concurrent.{Future, ExecutionContext}
import scala.util.Try
import scala.util.control.TailCalls.TailRec

trait Functor[F[_]] { F =>

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def compose[G[_]](implicit G: Functor[G]): Functor[({ type H[A] = F[G[A]] })#H] =
    new Functor[({ type H[A] = F[G[A]] })#H] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F.map(fga)(G.map(_)(f))
    }

  class FunctorOps[A](self: F[A]) {

    def map[B](f: A => B): F[B] = F.map(self)(f)

  }

}

object Functor {

  object Implicits {

    implicit def FunctorOps[A](self: A)(implicit A: Unify[Functor, A]): Functor[A.F]#FunctorOps[A.A] = new A.TC.FunctorOps(A.to(self))

  }

  implicit val Identity: Monad[Identity] with Traverse[Identity] = new Monad.IdentityMonad with Traverse.IdentityTraverse with Functor.IdentityFunctor {}

  implicit val Function0: Monad[Function0] with Traverse[Function0] = new Monad.Function0Monad with Traverse.Function0Traverse with Functor.Function0Functor {}

  implicit val Option: MonadPlus[Option] with Traverse[Option] = new MonadPlus.OptionMonadPlus with Traverse.OptionTraverse with Functor.OptionFunctor {}

  implicit val List: MonadPlus[List] with Traverse[List] = new MonadPlus.ListMonadPlus with Traverse.ListTraverse with Functor.ListFunctor {}

  implicit val Vector: MonadPlus[Vector] with Traverse[Vector] = new MonadPlus.VectorMonadPlus with Traverse.VectorTraverse with Functor.VectorFunctor {}

  implicit val IndexedSeq: MonadPlus[IndexedSeq] with Traverse[IndexedSeq] = new MonadPlus.IndexedSeqMonadPlus with Traverse.IndexedSeqTraverse with Functor.IndexedSeqFunctor {}

  implicit val Stream: MonadPlus[Stream] with Traverse[Stream] = new MonadPlus.StreamMonadPlus with Traverse.StreamTraverse with Functor.StreamFunctor {}

  implicit def Either[E]: Traverse[({ type F[A] = Either[E, A] })#F] = new Traverse.EitherTraverse[E] with Functor.EitherFunctor[E] {}

  implicit def Map[K]: Traverse[({ type F[A] = Map[K, A] })#F] = new Traverse.MapTraverse[K] with Functor.MapFunctor[K] {}

  implicit val Set: MonadPlus[Set] with Traverse[Set] = new MonadPlus.SetMonadPlus with Traverse.SetTraverse with Functor.SetFunctor {}

  implicit val Try: Monad[Try] = new Monad.TryMonad {}

  implicit def Future(implicit ec: ExecutionContext): Monad[Future] =
    new Monad.FutureMonad {
      val executor = ec
    }

  implicit val TailRec: Monad[TailRec] = new Monad.TailRecMonad {}

  trait IdentityFunctor extends Functor[Identity] {

    override final def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = f(fa)

  }

  trait Function0Functor extends Functor[Function0] {

    override final def map[A, B](fa: Function0[A])(f: A => B): Function0[B] = () => f(fa())

  }

  trait OptionFunctor extends Functor[Option] {

    override final def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)

  }

  trait ListFunctor extends Functor[List] {

    override final def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

  }

  trait VectorFunctor extends Functor[Vector] {

    override final def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)

  }

  trait IndexedSeqFunctor extends Functor[IndexedSeq] {

    override final def map[A, B](fa: IndexedSeq[A])(f: A => B): IndexedSeq[B] = fa.map(f)

  }

  trait StreamFunctor extends Functor[Stream] {

    override final def map[A, B](fa: Stream[A])(f: A => B): Stream[B] = fa.map(f)

  }

  trait EitherFunctor[E] extends Functor[({ type F[A] = Either[E, A] })#F] {

    override final def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa.right.map(f)

  }

  trait MapFunctor[K] extends Functor[({ type F[A] = Map[K, A] })#F] {

    override final def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.mapValues(f)

  }

  trait SetFunctor extends Functor[Set] {

    override final def map[A, B](fa: Set[A])(f: A => B): Set[B] = fa.map(f)

  }

}
