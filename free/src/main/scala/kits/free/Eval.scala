package kits.free

trait Eval { self =>

  type Succ[U]

  type Result[A]

  def eval[U, A](free: Free[Succ[U], A]): Free[U, Result[A]]

  final def apply[A](free: Free[Succ[Void], A]): Result[A] = andThen(Free.exec)(free)

  final def compose(that: Eval) = new Eval {
    type Succ[U] = that.Succ[self.Succ[U]]
    type Result[A] = self.Result[that.Result[A]]
    def eval[U, A](free: Free[Succ[U], A]): Free[U, Result[A]] = self.eval(that.eval(free))
  }

  final def andThen(that: Eval) = new Eval {
    type Succ[U] = self.Succ[that.Succ[U]]
    type Result[A] = that.Result[self.Result[A]]
    def eval[U, A](free: Free[Succ[U], A]): Free[U, Result[A]] = that.eval[U, self.Result[A]](self.eval(free))
  }

  final def andThen(that: Exec) = new Exec {
    type Succ[U] = self.Succ[that.Succ[U]]
    type Result[A] = that.Result[self.Result[A]]
    def exec[A](free: Free[Succ[Void], A]): Result[A] = that.exec(self.eval(free))
  }

}
