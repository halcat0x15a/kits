package kits

import kits.std._
import scala.collection.immutable.IndexedSeq

trait Monoid[A] {

  def empty: A

  def append(x: A, y: A): A

  def applicative: Applicative[({ type F[B] = A })#F] =
    new Applicative[({ type F[B] = A })#F] {
      def pure[B](b: B): A = empty
      def ap[B, C](fb: A)(f: A): A = append(f, fb)
    }

}

object Monoid {

  def append[A](x: A, y: A)(implicit A: Monoid[A]): A = A.append(x, y)

  implicit val String: Monoid[String] = new StringMonoid {}

  implicit val Unit: Monoid[Unit] = new UnitMonoid {}

  val Conj: Monoid[Boolean] = new ConjMonoid {}

  val Disj: Monoid[Boolean] = new DisjMonoid {}

  def Sum[A](implicit A: Numeric[A]): Monoid[A] = new SumMonoid[A] { val numeric = A }

  def Prod[A](implicit A: Numeric[A]): Monoid[A] = new ProdMonoid[A] { val numeric = A }

  implicit def Function0[A](implicit A: Monoid[A]): Monoid[() => A] = new Function0Monoid[A] { val monoid = A }

  implicit def Tuple2[A, B](implicit A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Tuple2Monoid[A, B] { val _1 = A; val _2 = B }

  implicit def Tuple3[A, B, C](implicit A: Monoid[A], B: Monoid[B], C: Monoid[C]): Monoid[(A, B, C)] = new Tuple3Monoid[A, B, C] { val _1 = A; val _2 = B; val _3 = C }

  implicit def Option[A](implicit A: Monoid[A]): Monoid[Option[A]] = new OptionMonoid[A] { val monoid = A }

  implicit def Map[K, V](implicit V: Monoid[V]): Monoid[Map[K, V]] = new MapMonoid[K, V] { val monoid = V }

  implicit def Ordering[A]: Monoid[Ordering[A]] = new OrderingMonoid[A] {}

  implicit def List[A]: Monoid[List[A]] = Functor.List.monoid[A]

  implicit def Vector[A]: Monoid[Vector[A]] = Functor.Vector.monoid[A]

  implicit def IndexedSeq[A]: Monoid[IndexedSeq[A]] = Functor.IndexedSeq.monoid[A]

  implicit def Stream[A]: Monoid[Stream[A]] = Functor.Stream.monoid[A]

  implicit def Set[A]: Monoid[Set[A]] = Functor.Set.monoid[A]

}
