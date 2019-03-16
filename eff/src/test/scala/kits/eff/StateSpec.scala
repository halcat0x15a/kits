package kits.eff

import org.scalatest.FlatSpec

class StateSpec extends FlatSpec {
  "State" should "get and set the state" in {
    val e = for {
      s <- State.get[Int]
      _ <- State.put(s + 1)
    } yield s
    assert(Eff.run(State.run(0)(e)) == (1, 0))
  }

  "transaction" should "protect the state" in {
    val e = for {
      _ <- State.modify((_: Int) + 1)
      _ <- Exc.fail("error")
    } yield ()
    assert(Eff.run(State.run(0)(Exc.run(e))) == (1, Left("error")))
    assert(Eff.run(State.run(0)(Exc.run(State[Int].transaction(e)))) == (0, Left("error")))
  }
}
