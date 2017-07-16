package kits.free

trait Handler { self =>

  type Cons[U]

  type Result[A]

  def apply[U, A](free: Free[Cons[U], A]): Free[U, Result[A]]

  final def run[A](free: Free[Cons[Void], A]): Result[A] = Free.run(apply(free))

  final def compose(that: Handler) = new Handler {
    type Cons[U] = that.Cons[self.Cons[U]]
    type Result[A] = self.Result[that.Result[A]]
    def apply[U, A](free: Free[Cons[U], A]): Free[U, Result[A]] = self(that(free))
  }

  final def andThen(that: Handler) = that.compose(this)

}
