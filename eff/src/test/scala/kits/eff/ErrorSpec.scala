package kits.eff

import org.scalatest.FlatSpec

class ErrorSpec extends FlatSpec {
  "Error" should "handle multiple errors" in {
    val e = for {
      _ <- Error.fail(42)
      _ <- Error.fail("hoge")
    } yield ()
    def runErrorString[R, A](eff: Eff[~[Error[String]] with R, A]): Eff[R, Either[String, A]] = Error.run[String, R, A](eff)
    Eff.run(Error.run(runErrorString(e)))
  }

  it should "recover from failures" in {
    val e = for {
      _ <- Error.fail(new IllegalArgumentException("hoge"))
    } yield "fuga"
    assert(Eff.run(Error.recover(e)(e => Eff.Pure(e.getMessage))) == "hoge")
  }
}
