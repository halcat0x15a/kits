package kits.free

sealed abstract class HList extends Product with Serializable {
  type Append[L <: HList] <: HList
}

case class HNil() extends HList {
  type Append[L <: HList] = L
}

case class :*:[H, T <: HList](head: Member[H, _], tail: T) extends HList {
  type Append[L <: HList] = :*:[H, T#Append[L]]
}

trait MemberList[L <: HList, U] {
  def list: L
}

object MemberList {

  implicit def nil[U]: MemberList[HNil, U] =
    new MemberList[HNil, U] {
      val list = HNil()
    }

  implicit def cons[F, U, L <: HList](implicit member: Member[F, U], members: MemberList[L, U]): MemberList[F :*: L, U] =
    new MemberList[F :*: L, U] {
      val list = :*:(member, members.list)
    }

  implicit def left[U, XS <: HList, YS <: HList](implicit members: MemberList[XS#Append[YS], U]): MemberList[XS, U] = ???

  implicit def right[U, XS <: HList, YS <: HList](implicit members: MemberList[XS#Append[YS], U]): MemberList[YS, U] = ???

  implicit def head[U, H, T <: HList](implicit members: MemberList[H :*: T, U]): Member[H, U] =
    members.list match {
      case :*:(h, _) => h.asInstanceOf[Member[H, U]]
    }

}
