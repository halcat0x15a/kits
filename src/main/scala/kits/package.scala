package object kits {

  type Identity[A] = A

  type Reader[R, A] = R => A

  type Writer[W, A] = (W, A)

  type State[S, A] = S => (S, A)

}
