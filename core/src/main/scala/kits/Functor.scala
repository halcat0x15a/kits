package kits

import kits.std._
import scala.collection.immutable.IndexedSeq
import scala.concurrent.{Future, ExecutionContext}
import scala.util.Try
import scala.util.control.TailCalls._

trait Functor[F[_]] { F =>

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def compose[G[_]](implicit G: Functor[G]): Functor[({ type H[A] = F[G[A]] })#H] =
    new Functor[({ type H[A] = F[G[A]] })#H] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F.map(fga)(G.map(_)(f))
    }

}

object Functor extends FunctorFunctions[Functor] {

  implicit val Identity: Monad[Identity] with Traverse[Identity] = new IdentityMonad with IdentityTraverse with IdentityFunctor {}

  implicit val Function0: Monad[Function0] with Traverse[Function0] = new Function0Monad with Function0Traverse with Function0Functor {}

  implicit val Option: MonadPlus[Option] with Traverse[Option] = new OptionMonadPlus with OptionTraverse with OptionFunctor {}

  implicit val List: MonadPlus[List] with Traverse[List] = new ListMonadPlus with ListTraverse with ListFunctor {}

  implicit val Vector: MonadPlus[Vector] with Traverse[Vector] = new VectorMonadPlus with VectorTraverse with VectorFunctor {}

  implicit val IndexedSeq: MonadPlus[IndexedSeq] with Traverse[IndexedSeq] = new IndexedSeqMonadPlus with IndexedSeqTraverse with IndexedSeqFunctor {}

  implicit val Stream: MonadPlus[Stream] with Traverse[Stream] = new StreamMonadPlus with StreamTraverse with StreamFunctor {}

  implicit def Either[E]: Traverse[({ type F[A] = Either[E, A] })#F] = new EitherTraverse[E] with EitherFunctor[E] {}

  implicit def Map[K]: Traverse[({ type F[A] = Map[K, A] })#F] = new MapTraverse[K] {}

  implicit val Set: MonadPlus[Set] with Traverse[Set] = new SetMonadPlus with SetTraverse with SetFunctor {}

  implicit val Try: Monad[Try] with Traverse[Try] = new TryMonad with TryTraverse with TryFunctor {}

  implicit def Future(implicit ec: ExecutionContext): Monad[Future] = new FutureMonad { val executor = ec }

  implicit val TailRec: Monad[TailRec] = new TailRecMonad {}

}

trait FunctorFunctions[T[F[_]] <: Functor[F]] {

  def map[F[_], A, B](fa: F[A])(f: A => B)(implicit F: T[F]): F[B] = F.map(fa)(f)

}
