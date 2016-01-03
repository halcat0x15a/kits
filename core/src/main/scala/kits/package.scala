package object kits {

  type Identity[A] = A

  type Conj = Boolean { type Conj }

  def Conj(bool: Boolean): Conj = bool.asInstanceOf[Conj]

  type Disj = Boolean { type Disj }

  def Disj(bool: Boolean): Disj = bool.asInstanceOf[Disj]

  type Sum[A] = A { type Sum }

  def Sum[A](a: A): Sum[A] = a.asInstanceOf[Sum[A]]

  type Prod[A] = A { type Prod }

  def Prod[A](a: A): Prod[A] = a.asInstanceOf[Prod[A]]

}
