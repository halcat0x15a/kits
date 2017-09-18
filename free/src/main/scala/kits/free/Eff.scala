package kits.free

trait Eff[A] { self =>

  type Effects <: HTree

  def free[U](implicit members: MemberSet[Effects, U]): Free[U, A]

  def map[B](f: A => B) = new Eff[B] {
    type Effects = self.Effects
    def free[U](implicit members: MemberSet[Effects, U]) = self.free[U].map(f)
  }

  def flatMap[B, L <: HTree](f: A => Eff[B] { type Effects = L }) = new Eff[B] {
    type Effects = self.Effects :*: L
    def free[U](implicit members: MemberSet[Effects, U]): Free[U, B] =
      self.free[U](MemberSet.left).flatMap(a => f(a).free[U](MemberSet.right))
  }

}

object Eff {

  def ask[R] = new Eff[R] {
    type Effects = HLeaf[Reader[R]]
    def free[U](implicit members: MemberSet[Effects, U]): Free[U, R] = Reader.ask(MemberSet.get)
  }

  def tell[W](w: W) = new Eff[Unit] {
    type Effects = HLeaf[Writer[W]]
    def free[U](implicit members: MemberSet[Effects, U]): Free[U, Unit] = Writer.tell(w)(MemberSet.get)
  }

}

object Main extends App {
  import Eff._
    println((Writer.handle[String] compose Reader.handle("hoge")).run {
    (for { x <- ask[String]; _ <- tell(x); _ <- tell(x); _ <- ask[String] } yield x).free
  })
}
