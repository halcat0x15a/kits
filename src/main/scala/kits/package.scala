package object kits {

  type Identity[A] = A

  type State[S, A] = S => (S, A)

}
