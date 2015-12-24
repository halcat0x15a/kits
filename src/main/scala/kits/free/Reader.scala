package kits.free

sealed abstract class Reader[A]

case class Get[A]() extends Reader[String]

object Reader {

  def run[U <: Union, A](free: Free[Reader :+: U, A], a: String): Free[U, A] =
    Free.fold(free)(a => Pure(a))(new Free.Fold[Reader, U, A] {
      def apply[T](fa: Reader[T])(k: T => Free[Trampoline :+: U, A]): Free[Trampoline :+: U, A] =
        fa match {
          case Get() => k(a)
        }
    })

  def ask[U <: Union, A](implicit member: Member[Reader, U]): Free[U, A] =
    Impure(member.inject(Get()), Leaf((x: A) => Pure(x)))

}
