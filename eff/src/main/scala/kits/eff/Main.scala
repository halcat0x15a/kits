package kits.eff

object Main extends App {
  val foo: Eff[~[Reader[Int]] with ~[Error[String]] with ~[Writer[String]], Int] = for {
    x <- Reader.ask[Int]
    _ <- Writer.tell(x.toString)
    y <- Either.cond(x > 0, x, "hoge").lift
  } yield x + y

  println(Eff.run(Error.runEither(Writer.runList(Reader.run(1)(foo)))))
}
