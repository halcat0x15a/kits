package kits.free

trait Run { self =>

  type Sum[U <: Union] <: Union

  type F[A]

  def run[U <: Union, A](free: Free[Sum[U], A]): Free[U, F[A]]

  def apply[U <: Union, A](free: Free[Sum[Void], A]): F[A] =
    (run(free): @unchecked) match {
      case Pure(a) => a
    }


  def compose(that: Run) = new Run {
    type Sum[U <: Union] = that.Sum[self.Sum[U]]
    type F[A] = self.F[that.F[A]]
    def run[U <: Union, A](free: Free[Sum[U], A]): Free[U, F[A]] = self.run(that.run(free))
  }

  def andThen(that: Run) = new Run {
    type Sum[U <: Union] = self.Sum[that.Sum[U]]
    type F[A] = that.F[self.F[A]]
    def run[U <: Union, A](free: Free[Sum[U], A]): Free[U, F[A]] = that.run(self.run(free))
  }

}
