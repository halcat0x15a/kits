package kits.eff

import org.scalatest.FlatSpec

class WriterSpec extends FlatSpec {
  "Writer" should "produce output" in {
    val e = for {
      _ <- Writer.tell("hoge")
      _ <- Writer.tell("fuga")
    } yield ()
    assert(Eff.run(Writer.run(e)) == (Vector("hoge", "fuga"), ()))
  }

  it should "fold output" in {
    val e = for {
      _ <- Writer.tell("hoge")
      _ <- Writer.tell("fuga")
    } yield ()
    assert(Eff.run(Writer.fold(e)("")(_ + _)) == ("hogefuga", ()))
  }

  it should "handle multiple output" in {
    val e = for {
      _ <- Writer.tell(42)
      _ <- Writer.tell("hoge")
    } yield ()
    assert(Eff.run(Writer[String].run(Writer[Int].run(e))) == (Vector("hoge"), (Vector(42), ())))
    assert(Eff.run(Writer[Int].run(Writer[String].run(e))) == (Vector(42), (Vector("hoge"), ())))
  }
}
