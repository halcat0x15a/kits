package kits.free

object Example extends App {

  type ReaderInt[A] = Reader[Int, A]

  type WriterString[A] = Writer[String, A]

  def e1[U <: Union](implicit w: Member[WriterString, U], r: Member[ReaderInt, U]): Free[U, Unit] = for {
    _ <- Writer.tell("start")
    a <- Reader.ask
    _ <- Writer.tell(a.toString)
    _ <- Writer.tell("end")
  } yield ()

  def e2[U <: Union](n: Int)(implicit w: Member[WriterString, U], r: Member[ReaderInt, U]): Free[U, Unit] =
    if (n <= 0) {
      Writer.tell("end")
    } else {
      for {
        a <- Reader.ask
        _ <- Writer.tell(a.toString)
        _ <- e2(n - 1)
      } yield ()
    }

  println(Free.run(Reader.run(Writer.run(e1[WriterString :+: ReaderInt :+: Void]), 42)))
  println(Free.run(Writer.run(Reader.run(e2[ReaderInt :+: WriterString :+: Void](100000), 42)))._2.size)

}
