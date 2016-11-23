package kits

package std

trait VectorFunctor extends Functor[Vector] {

  override final def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)

}

trait VectorMonadPlus extends MonadPlus[Vector] { self: VectorFunctor =>

  override final def zero[A]: Vector[A] = Vector.empty

  override final def pure[A](a: A): Vector[A] = Vector(a)

  override final def plus[A](x: Vector[A], y: Vector[A]): Vector[A] = x ++ y

  override final def filter[A](fa: Vector[A])(p: A => Boolean): Vector[A] = fa.filter(p)

  override final def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)

  override final def flatten[A](ffa: Vector[Vector[A]]): Vector[A] = ffa.flatten
}

trait VectorTraverse extends Traverse[Vector] { self: VectorFunctor =>

  override final def traverse[F[_], A, B](fa: Vector[A])(f: A => F[B])(implicit F: Applicative[F]): F[Vector[B]] =
    fa.foldLeft(F.pure(Vector.empty[B]))((ga, a) => F.map2(ga, f(a))(_ :+ _))

}
