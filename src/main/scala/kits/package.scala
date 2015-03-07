package object kits {
  type Id[A] = A
  implicit val id = new Monad[Id] {
    def pure[A](a: A): Id[A] = a
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }
  type Const[A, B] = A
  implicit def const[A](implicit A: Monoid[A]) = new Applicative[({ type F[B] = Const[A, B] })#F] {
    def pure[B](b: B): Const[A, B] = A.zero
    def apply[B, C](fb: Const[A, B])(f: Const[A, B => C]): Const[A, C] = A.append(fb, f)
  }
}
