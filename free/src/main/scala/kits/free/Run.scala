package kits.free

trait Run { self =>

  type Sum[U]

  type F[A]

  def run[U, A](free: Free[Sum[U], A]): Free[U, F[A]]

  final def apply[A](free: Free[Sum[Void], A]): F[A] = andThen(Free.exec)(free)

  def compose(that: Run) = new Run {
    type Sum[U] = that.Sum[self.Sum[U]]
    type F[A] = self.F[that.F[A]]
    def run[U, A](free: Free[Sum[U], A]): Free[U, F[A]] = self.run(that.run(free))
  }

  def andThen(that: Run) = new Run {
    type Sum[U] = self.Sum[that.Sum[U]]
    type F[A] = that.F[self.F[A]]
    def run[U, A](free: Free[Sum[U], A]): Free[U, F[A]] = that.run(self.run(free))
  }

  def andThen(that: Exec) = new Exec {
    type Sum[U] = self.Sum[that.Sum[U]]
    type F[A] = that.F[self.F[A]]
    def exec[A](free: Free[Sum[Void], A]): F[A] = that.exec(self.run(free))
  }

}
