package kits

import scala.collection.immutable.IndexedSeq

trait Traverse[F[_]] extends Functor[F] { F =>

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = traverse(fa)(a => f(a): Identity[B])

  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

  def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B = B.applicative.traverse(fa)(f)(F)

  def fold[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type H[A] = F[G[A]] })#H] =
    new Traverse[({ type H[A] = F[G[A]] })#H] {
      def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        F.traverse(fga)(G.traverse(_)(f))
    }

}

object Traverse {

  def traverse[F[_], G[_]: Applicative, A, B](fa: F[A])(f: A => G[B])(implicit F: Traverse[F]): G[F[B]] = F.traverse(fa)(f)

  def sequence[F[_], G[_]: Applicative, A](fga: F[G[A]])(implicit F: Traverse[F]): G[F[A]] = F.sequence(fga)

  def foldMap[F[_], A, B: Monoid](fa: F[A])(f: A => B)(implicit F: Traverse[F]): B = F.foldMap(fa)(f)

  def fold[F[_], A: Monoid](fa: F[A])(implicit F: Traverse[F]): A = F.fold(fa)

  trait IdentityTraverse extends Traverse[Identity] { self: Functor.IdentityFunctor =>
    override final def traverse[F[_]: Applicative, A, B](fa: Identity[A])(f: A => F[B]): F[Identity[B]] = f(fa)
  }

  trait Function0Traverse extends Traverse[Function0] { self: Functor.Function0Functor =>
    override final def traverse[F[_], A, B](fa: Function0[A])(f: A => F[B])(implicit F: Applicative[F]): F[Function0[B]] = F.map(f(fa()))(b => () => b)
  }

  trait OptionTraverse extends Traverse[Option] { self: Functor.OptionFunctor =>
    override final def traverse[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]): F[Option[B]] =
      fa.fold(F.pure(Option.empty[B]))(a => F.map(f(a))(Some(_)))
  }

  trait ListTraverse extends Traverse[List] { self: Functor.ListFunctor =>
    override final def traverse[F[_], A, B](fa: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
      fa.foldRight(F.pure(List.empty[B]))((a, ga) => F.map2(f(a), ga)(_ :: _))
  }

  trait VectorTraverse extends Traverse[Vector] { self: Functor.VectorFunctor =>
    override final def traverse[F[_], A, B](fa: Vector[A])(f: A => F[B])(implicit F: Applicative[F]): F[Vector[B]] =
      fa.foldLeft(F.pure(Vector.empty[B]))((ga, a) => F.map2(ga, f(a))(_ :+ _))
  }

  trait IndexedSeqTraverse extends Traverse[IndexedSeq] { self: Functor.IndexedSeqFunctor =>
    override final def traverse[F[_], A, B](fa: IndexedSeq[A])(f: A => F[B])(implicit F: Applicative[F]): F[IndexedSeq[B]] =
      fa.foldLeft(F.pure(IndexedSeq.empty[B]))((ga, a) => F.map2(ga, f(a))(_ :+ _))
  }

  trait StreamTraverse extends Traverse[Stream] { self: Functor.StreamFunctor =>
    override final def traverse[F[_], A, B](fa: Stream[A])(f: A => F[B])(implicit F: Applicative[F]): F[Stream[B]] =
      fa.foldRight(F.pure(Stream.empty[B]))((a, ga) => F.map2(f(a), ga)(_ #:: _))
  }

  trait EitherTraverse[E] extends Traverse[({ type F[A] = Either[E, A] })#F] { self: Functor.EitherFunctor[E] =>
    override final def traverse[F[_], A, B](fa: Either[E, A])(f: A => F[B])(implicit F: Applicative[F]): F[Either[E, B]] =
      fa.fold(e => F.pure(Left(e)), a => F.map(f(a))(Right(_)))
  }

  trait MapTraverse[K] extends Traverse[({ type F[A] = Map[K, A] })#F] { self: Functor.MapFunctor[K] =>
    override final def traverse[F[_], A, B](fa: Map[K, A])(f: A => F[B])(implicit F: Applicative[F]): F[Map[K, B]] =
      fa.foldLeft(F.pure(scala.collection.immutable.Map.empty[K, B])) { case (ga, (k, a)) => F.map2(ga, f(a))((a, b) => a + (k -> b)) }
  }

  trait SetTraverse extends Traverse[Set] { self: Functor.SetFunctor =>
    override final def traverse[F[_], A, B](fa: Set[A])(f: A => F[B])(implicit F: Applicative[F]): F[Set[B]] =
      fa.foldLeft(F.pure(Set.empty[B]))((ga, a) => F.map2(ga, f(a))(_ + _))
  }

}
