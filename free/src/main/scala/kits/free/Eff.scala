package kits.free

trait Eff[A] { self =>

  type Constraints <: HList

  def free[U](implicit members: MemberList[Constraints, U]): Free[U, A]

  def map[B](f: A => B): Eff[B] { type Constraints = self.Constraints } = ???

  def flatMap[B, L <: HList](f: A => Eff[B] { type Constraints = L }) = new Eff[B] {
    type Constraints = self.Constraints#Append[L]
    def free[U](implicit members: MemberList[Constraints, U]): Free[U, B] =
      self.free[U].flatMap(a => f(a).free[U])
  }

}

object Eff {

  def ask[R]: Eff[R] { type Constraints = :*:[Reader[R], HNil] } = new Eff[R] {
    type Constraints = :*:[Reader[R], HNil]
    def free[U](implicit members: MemberList[Constraints, U]): Free[U, R] = Reader.ask[U, R](MemberList.head)
  }

  def tell[W](w: W): Eff[Unit] { type Constraints = :*:[Writer[W], HNil] } = new Eff[Unit] {
    type Constraints = :*:[Writer[W], HNil]
    def free[U](implicit members: MemberList[Constraints, U]): Free[U, Unit] = Writer.tell(w)(MemberList.head)
  }

}

object Main {
  import Eff._
  type U = Reader[String] :+: Writer[String] :+: Void
  (for { x <- ask[String]; _ <- tell(x); _ <- tell(x); _ <- ask[String] } yield x).free[U]
}
