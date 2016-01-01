package kits.free

object Example extends App {

  object ReaderInt extends Reader[Int]

  object WriterString extends Writer[String]

  def e1[U <: Union](implicit w: Member[WriterString.Writer, U], r: Member[ReaderInt.Reader, U]): Free[U, Unit] = for {
    _ <- WriterString.tell("start")
    a <- ReaderInt.ask
    _ <- WriterString.tell(a.toString)
    _ <- WriterString.tell("end")
  } yield ()

  def e2[U <: Union](n: Int)(implicit w: Member[WriterString.Writer, U], r: Member[ReaderInt.Reader, U]): Free[U, Unit] =
    if (n <= 0) {
      WriterString.tell("end")
    } else {
      for {
        a <- ReaderInt.ask
        _ <- WriterString.tell(a.toString)
        _ <- e2(n - 1)
      } yield ()
    }

  println(Free.run(ReaderInt.run(WriterString.run(e1[WriterString.Writer :+: ReaderInt.Reader :+: Void]), 42)))
  println(Free.run(WriterString.run(ReaderInt.run(e2[ReaderInt.Reader :+: WriterString.Writer :+: Void](100000), 42)))._2.size)

}
