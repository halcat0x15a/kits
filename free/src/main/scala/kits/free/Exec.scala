package kits.free

trait Exec { self =>

  type Sum[U]

  type F[A]

  def exec[A](free: Free[Sum[Void], A]): F[A]

  final def apply[A](free: Free[Sum[Void], A]): F[A] = exec(free)

  def compose(that: Run) = new Exec {
    type Sum[U] = that.Sum[self.Sum[U]]
    type F[A] = self.F[that.F[A]]
    def exec[A](free: Free[Sum[Void], A]): F[A] = self.exec(that.run(free))
  }

}
