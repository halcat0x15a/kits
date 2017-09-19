package kits.free

trait Eff[A] { self =>

  type Effects <: EffTree

  def free[U](implicit members: EffMember[Effects, U]): Free[U, A]

  def map[B](f: A => B): Eff[B] { type Effects = self.Effects } =
    new Eff[B] {
      type Effects = self.Effects
      def free[U](implicit members: EffMember[Effects, U]) = self.free[U].map(f)
    }

  def flatMap[B, Effs <: EffTree](f: A => Eff[B] { type Effects = Effs }): Eff[B] { type Effects = self.Effects :*: Effs } =
    new Eff[B] {
      type Effects = self.Effects :*: Effs
      def free[U](implicit members: EffMember[Effects, U]): Free[U, B] =
        self.free[U](EffMember.left).flatMap(a => f(a).free[U](EffMember.right))
    }

}

sealed abstract class EffTree extends Product with Serializable

case class EffLeaf[F](member: Member[F, _]) extends EffTree

case class :*:[L <: EffTree, R <: EffTree](left: L, right: R) extends EffTree

trait EffMember[Effs <: EffTree, U] {

  def list: Effs

  def get[F](implicit get: GetMember[Effs, F]): Member[F, U] = get(this)

}

object EffMember {

  def apply[Effs <: EffTree, U](effs: Effs): EffMember[Effs, U] =
    new EffMember[Effs, U] {
      val list = effs
    }

  def right[XS <: EffTree, YS <: EffTree, U](implicit members: EffMember[XS :*: YS, U]): EffMember[YS, U] =
    members.list match {
      case _ :*: right => EffMember(right)
    }

  def left[XS <: EffTree, YS <: EffTree, U](implicit members: EffMember[XS :*: YS, U]): EffMember[XS, U] =
    members.list match {
      case left :*: _ => EffMember(left)
    }

  implicit def Leaf[F, U](implicit member: Member[F, U]): EffMember[EffLeaf[F], U] =
    EffMember(EffLeaf(member))

  implicit def Node[L <: EffTree, R <: EffTree, U](implicit left: EffMember[L, U], right: EffMember[R, U]): EffMember[L :*: R, U] =
    EffMember[L :*: R, U](:*:(left.list, right.list))

}

trait GetMember[Effs <: EffTree, F] {

  def apply[U](member: EffMember[Effs, U]): Member[F, U]

}

trait LowPriorityGetMember {

  implicit def Right[L <: EffTree, R <: EffTree, F](implicit get: GetMember[R, F]): GetMember[L :*: R, F] =
    new GetMember[L :*: R, F] {
      def apply[U](members: EffMember[L :*: R, U]): Member[F, U] =
        members.list match {
          case _ :*: r => get(EffMember(r))
        }
    }

}

object GetMember extends LowPriorityGetMember {

  implicit def Leaf[F]: GetMember[EffLeaf[F], F] =
    new GetMember[EffLeaf[F], F] {
      def apply[U](members: EffMember[EffLeaf[F], U]): Member[F, U] = members.list.member.asInstanceOf[Member[F, U]]
    }

  implicit def Left[L <: EffTree, R <: EffTree, F](implicit get: GetMember[L, F]): GetMember[L :*: R, F] =
    new GetMember[L :*: R, F] {
      def apply[U](members: EffMember[L :*: R, U]): Member[F, U] =
        members.list match {
          case l :*: _ => get(EffMember(l))
        }
    }

}

object Test {
  val eff = for {
    x <- Reader.ask[String]
    _ <- Writer.tell(x)
  } yield x

  val eff0 = Reader.local(eff)((_: String) + "bar")

  implicitly[eff.Effects =:= (EffLeaf[Reader[String]] :*: EffLeaf[Writer[String]])]

  implicitly[eff0.Effects =:= (EffLeaf[Reader[String]] :*: EffLeaf[Reader[String]] :*: EffLeaf[Writer[String]])]

  (Reader.handle("foo") compose Writer.handle[String]).run(eff0.free)
}
