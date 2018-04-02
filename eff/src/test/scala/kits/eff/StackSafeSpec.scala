package kits.eff

import org.scalatest.FlatSpec

class StackSafeSpec extends FlatSpec {
  "Eff" should "count up to 1,000,000 with State" in {
    val N = 1000000
    def loop: Eff[~[Reader[Int]] with ~[State[Int]], Unit] =
      for {
        s <- State.get[Int]
        _ <- if (s >= N) {
          Eff.Pure(s)
        } else {
          State.put(s + 1).flatMap(_ => loop)
        }
      } yield ()
    assert(Eff.run(State.run(0)(Reader.run(0)(loop))) == (N, ()))
  }
}
