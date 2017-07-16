package kits

package object free {

  type Arrows[U, A, B] = Vector[A => Free[U, B]]

  type Maybe = Error[Unit]

}
