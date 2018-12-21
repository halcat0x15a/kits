package kits.eff

import org.scalatest.FlatSpec

class StateSpec extends FlatSpec {
  "transaction" should "protect a state" in {
    val e = for {
      _ <- State.modify((_: Int) + 1)
      _ <- Exc.fail("err")
    } yield ()
    assert(Eff.run(State.run(0)(Exc[String].run(e))) == (1, Left("err")))
    assert(Eff.run(State.run(0)(Exc[String].run(State[Int].transaction(e)))) == (0, Left("err")))
  }
}
