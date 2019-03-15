package kits.eff

import org.scalatest.FlatSpec

class EffSpec extends FlatSpec {
  "Eff" should "not stack overflow even if it counts up to 1,000,000 in Reader and State" in {
    val N = 1000000
    def loop: Eff[Reader[Int] with State[Int], Unit] =
      for {
        n <- Reader.ask[Int]
        s <- State.get[Int]
        _ <- if (s < n) {
          State.put(s + 1).flatMap(_ => loop)
        } else {
          Eff.Pure(s)
        }
      } yield ()
    val s = System.nanoTime
    assert(Eff.run(State.run(0)(Reader.run(N)(loop))) == (N, ()))
    println((System.nanoTime - s) / 1000000000.0)
  }

  it should "filter the value with Opt" in {
    val e = for {
      n <- Reader.ask[Int]
      if n > 0
    } yield n
    assert(Eff.run(Opt.run(Reader.run(1)(e))) == Some(1))
    assert(Eff.run(Opt.run(Reader.run(0)(e))) == None)
  }
}
