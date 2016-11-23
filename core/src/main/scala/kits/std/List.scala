package kits

package std

trait ListFunctor extends Functor[List] {

  override final def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)

}

trait ListMonadPlus extends MonadPlus[List] { self: ListFunctor =>

  override final def zero[A]: List[A] = Nil

  override final def pure[A](a: A): List[A] = List(a)

  override final def plus[A](x: List[A], y: List[A]): List[A] = x ::: y

  override final def filter[A](fa: List[A])(p: A => Boolean): List[A] = fa.filter(p)

  override final def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

  override final def flatten[A](ffa: List[List[A]]): List[A] = ffa.flatten

}

trait ListTraverse extends Traverse[List] { self: ListFunctor =>

  override final def traverse[F[_], A, B](fa: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] =
    fa.foldRight(F.pure(List.empty[B]))((a, ga) => F.map2(f(a), ga)(_ :: _))

}
