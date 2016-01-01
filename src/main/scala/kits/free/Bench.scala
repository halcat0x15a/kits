package kits.free

object Bench extends App {

  type ReaderString[A] = Reader[String, A]

  type WriterString[A] = Writer[String, A]

  def e1[U <: Union](n: Int)(implicit r: Member[ReaderString, U], w: Member[WriterString, U]): Free[U, Unit] =
    if (n <= 0) {
      Writer.tell("end")
    } else {
      for {
        a <- Reader.ask
        _ <- Writer.tell(a)
        _ <- e1(n - 1)
      } yield ()
    }

  def e2(n: Int): scalaz.Kleisli[({ type F[A] = scalaz.WriterT[scalaz.Free.Trampoline, String, A] })#F, String, Unit] = {
    import scalaz._, Scalaz._
    implicit val m = WriterT.writerTMonad[Free.Trampoline, String]
    type T[F[_], A] = ReaderT[F, String, A]
    type F[A] = WriterT[Free.Trampoline, String, A]
    if (n <= 0) {
      WriterT.writerT(Trampoline.done(("end", ()))).liftM[T]
    } else {
      for {
        a <- ReaderT.ask[F, String]
        _ <- WriterT.writerT(Trampoline.done((a, ()))).liftM[T]
        _ <- e2(n - 1)
      } yield ()
    }
  }

  val r1 = for (n <- 1 to 1000) yield {
    val s = System.nanoTime
    Free.run(Writer.run(Reader.run(e1[ReaderString :+: WriterString :+: Void](n), "hoge")))
    System.nanoTime - s
  }

  val r2 = for (n <- 1 to 1000) yield {
    val s = System.nanoTime
    e2(n).run("hoge").run.run
    System.nanoTime - s
  }

  println(r1.takeRight(100).sum.toDouble / 100000000)
  println(r2.takeRight(100).sum.toDouble / 100000000)

}
