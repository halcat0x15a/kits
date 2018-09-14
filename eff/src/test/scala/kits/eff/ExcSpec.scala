package kits.eff

import org.scalatest.FlatSpec

class ExcSpec extends FlatSpec {
  "Error" should "handle multiple errors" in {
    val e = for {
      _ <- Exc.fail(42)
      _ <- Exc.fail("hoge")
    } yield ()
    assert(Eff.run(Exc.run(Exc.run[String, Exc[Int], Unit](e))) == Left(42))
  }

  it should "recover from failures" in {
    val e = for {
      _ <- Exc.fail(new IllegalArgumentException("hoge"))
    } yield "fuga"
    assert(Eff.run(Exc.recover(e)(e => Eff.Pure(e.getMessage))) == "hoge")
  }
}
