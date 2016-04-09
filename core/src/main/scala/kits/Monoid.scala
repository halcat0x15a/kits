package kits

trait Monoid[A] extends Semigroup[A] {

  def empty: A

  implicit val applicative: Applicative[({ type F[B] = A })#F] =
    new Applicative[({ type F[B] = A })#F] {
      def pure[B](b: B): A = empty
      def ap[B, C](fb: A)(f: A): A = append(f, fb)
    }

}
