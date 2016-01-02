package kits.free

package example

import org.scalatest.FunSuite

class FreeExample extends FunSuite {

  type ReaderInt[A] = Reader[Int, A]

  def add1[U <: Union](implicit r: Member[ReaderInt, U]): Free[U, Int] =
    for {
      a <- Reader.ask
    } yield a + 1

  test("ask") {
    assert(Free.run(Reader.run(add1[ReaderInt :+: Void], 10)) == 11)
  }

  def add11[U <: Union](implicit r: Member[ReaderInt, U]): Free[U, Int] =
    Reader.local(add1)(((_: Int) + 10))

  test("local") {
    assert(Free.run(Reader.run(add11[ReaderInt :+: Void], 10)) == 21)
  }

  type WriterString[A] = Writer[String, A]

  def rdwr[U <: Union](implicit r: Member[ReaderInt, U], w: Member[WriterString, U]): Free[U, Int] =
    for {
      _ <- Writer.tell("begin")
      n <- add11
      _ <- Writer.tell("end")
    } yield n

  test("tell") {
    assert(Free.run(Writer.run(Reader.run(rdwr[ReaderInt :+: WriterString :+: Void], 10))) == (Vector("begin", "end"), 21))
  }

}
