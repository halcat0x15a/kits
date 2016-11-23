package kits

package std

trait MapTraverse[K] extends Traverse[({ type F[A] = Map[K, A] })#F] {

  override final def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.mapValues(f)

  override final def traverse[F[_], A, B](fa: Map[K, A])(f: A => F[B])(implicit F: Applicative[F]): F[Map[K, B]] =
      fa.foldLeft(F.pure(scala.collection.immutable.Map.empty[K, B])) { case (ga, (k, a)) => F.map2(ga, f(a))((a, b) => a + (k -> b)) }

}

trait MapMonoid[K, V] extends Monoid[Map[K, V]] {

  def monoid: Monoid[V]

  override final def empty: Map[K, V] = scala.collection.immutable.Map.empty

  override final def append(x: Map[K, V], y: Map[K, V]): Map[K, V] =
    x.foldLeft(y) {
      case (a, (k, v)) => a.updated(k, a.get(k).fold(v)(monoid.append(v, _)))
    }

}
