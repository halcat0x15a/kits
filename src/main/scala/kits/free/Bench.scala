package kits.free

import org.openjdk.jmh.annotations.Benchmark

class Bench {

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

  def e3(n: Int): scalaz.ReaderWriterStateT[scalaz.Free.Trampoline, String, String, Unit, Unit] = {
    import scalaz._, Scalaz._
    if (n <= 0) {
      ReaderWriterStateT((r: String, s: Unit) => Trampoline.done(("end", (), s)))
    } else {
      for {
        a <- ReaderWriterStateT((r: String, s: Unit) => Trampoline.done(("", r, s)))
        _ <- ReaderWriterStateT((r: String, s: Unit) => Trampoline.done((r, (), s)))
        _ <- e3(n - 1)
      } yield ()
    }
  }

  def r1(n: Int): String = Free.run(Writer.run(Reader.run(e1[ReaderString :+: WriterString :+: Void](n), "hoge")))._2

  @Benchmark
  def r1_1000: String = r1(1000)
  @Benchmark
  def r1_100: String = r1(100)

  def r2(n: Int) = e2(n).run("hoge").run.run._1

  @Benchmark
  def r2_1000: String = r2(1000)
  @Benchmark
  def r2_100: String = r2(100)

  def r3(n: Int): String = e3(n).run("hoge", ()).run._1

  @Benchmark
  def r3_1000: String = r3(1000)
  @Benchmark
  def r3_100: String = r3(100)

}

object Bench {
  def main(args: Array[String]): Unit = {
    val bench = new Bench
    import bench._
    println(r3_100)
    println(r2_100)
    println(r1_100)
  }
}
