package kits.eff

import org.scalatest.FlatSpec

class ReaderSpec extends FlatSpec {
  "Reader" should "handle multiple input" in {
    val e = for {
      i <- Reader.ask[Int]
      s <- Reader.ask[String]
    } yield i + s
    assert(Eff.run(Reader.run(42)(Reader.run("hoge")(e))) == "42hoge")
  }
}
