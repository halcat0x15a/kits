package kits.free

sealed abstract class HTree extends Product with Serializable

case class HLeaf[F](member: Member[F, _]) extends HTree

case class :*:[L <: HTree, R <: HTree](left: L, right: R) extends HTree

case class MemberSet[L <: HTree, U](list: L)

object MemberSet {

  def get[F, U](implicit members: MemberSet[HLeaf[F], U]): Member[F, U] = members.list.member.asInstanceOf[Member[F, U]]

  def left[U, XS <: HTree, YS <: HTree](implicit members: MemberSet[XS :*: YS, U]): MemberSet[XS, U] =
    members.list match {
      case left :*: _ => MemberSet(left)
    }

  def right[U, XS <: HTree, YS <: HTree](implicit members: MemberSet[XS :*: YS, U]): MemberSet[YS, U] =
    members.list match {
      case _ :*: right => MemberSet(right)
    }

  implicit def Leaf[F, U](implicit member: Member[F, U]): MemberSet[HLeaf[F], U] =
    MemberSet(HLeaf(member))

  implicit def Node[U, L <: HTree, R <: HTree](implicit left: MemberSet[L, U], right: MemberSet[R, U]): MemberSet[L :*: R, U] =
    MemberSet[L :*: R, U](:*:(left.list, right.list))

}
