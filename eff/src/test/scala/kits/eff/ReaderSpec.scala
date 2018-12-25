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

  "local" should "modify the environment" in {
    val e = for {
      i <- Reader.ask[Int]
    } yield i / 2
    assert(Eff.run(Reader.run(42)(Reader.local((_: Int) * 2)(e))) == 42)
  }
}
