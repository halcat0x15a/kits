package kits.eff

object Main extends App {
  val foo: Eff[~[Reader[Int]] with ~[Writer[String]], Int] = for {
    x <- Reader.ask[Int]
    _ <- Writer.tell(x.toString)
  } yield x

  println(Eff.run(Writer.run(Reader.run(0)(foo))))
}
