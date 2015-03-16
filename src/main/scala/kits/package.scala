package object kits {
  type Id[A] = A
  type Const[A, B] = A
  type Comp[F[_], G[_], A] = F[G[A]]
}
