package kits.free

sealed abstract class EffTree extends Product with Serializable {

  type Union

}

sealed abstract case class EffEmpty() extends EffTree

sealed abstract case class EffLeaf[F]() extends EffTree {

  def member: Member[F, Union]

}

sealed abstract case class :*:[L, R]() extends EffTree { self =>

  def left: L { type Union = self.Union }

  def right: R { type Union = self.Union }

}

object EffTree {

  implicit def EffEmpty[U]: EffEmpty { type Union = U } =
    new EffEmpty {
      type Union = U
    }

  implicit def EffLeaf[F, U](implicit m: Member[F, U]): EffLeaf[F] { type Union = U } =
    new EffLeaf[F] {
      type Union = U
      val member = m
    }

  implicit def EffNode[L, R, U](implicit l: L { type Union = U }, r: R { type Union = U }): (L :*: R) { type Union = U } =
    new (L :*: R) {
      type Union = U
      val left = l
      val right = r
    }

}

trait GetMember[F, Effs] {

  def apply[U](effs: Effs { type Union = U }): Member[F, U]

}

trait LowPriorityGetMember {

  implicit def Right[F, L, R](implicit get: GetMember[F, R]): GetMember[F, L :*: R] =
    new GetMember[F, L :*: R] {
      def apply[U](effs: (L :*: R) { type Union = U }): Member[F, U] = get(effs.right)
    }

}

object GetMember extends LowPriorityGetMember {

  implicit def Leaf[F]: GetMember[F, EffLeaf[F]] =
    new GetMember[F, EffLeaf[F]] {
      def apply[U](effs: EffLeaf[F] { type Union = U }): Member[F, U] = effs.member
    }

  implicit def Left[F, L, R](implicit get: GetMember[F, L]): GetMember[F, L :*: R] =
    new GetMember[F, L :*: R] {
      def apply[U](effs: (L :*: R) { type Union = U }): Member[F, U] = get(effs.left)
    }

}
