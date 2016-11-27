package kits.free

trait Exec { self =>

  type Succ[U]

  type Result[A]

  def exec[A](free: Free[Succ[Void], A]): Result[A]

  final def apply[A](free: Free[Succ[Void], A]): Result[A] = exec(free)

  final def compose(that: Eval) = new Exec {
    type Succ[U] = that.Succ[self.Succ[U]]
    type Result[A] = self.Result[that.Result[A]]
    def exec[A](free: Free[Succ[Void], A]): Result[A] = self.exec(that.eval(free))
  }

}
