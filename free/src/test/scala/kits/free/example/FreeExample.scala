package kits

package free

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
    assert(Free.run(Writer.run(Reader.run(rdwr[ReaderInt :+: WriterString :+: Void], 10))) == ("beginend", 21))
  }

  type ErrorInt[A] = Error[Int, A]

  def tooBig[U <: Union](n: Int)(implicit e: Member[ErrorInt, U]): Free[U, Int] =
    if (n > 5)
      Error.fail(n)
    else
      Pure(n)

  test("error") {
    assert(Free.run(Error.run(tooBig[ErrorInt :+: Void](3))) == Right(3))
    assert(Free.run(Error.run(tooBig[ErrorInt :+: Void](7))) == Left(7))
  }

  type StateInt[A] = State[Int, A]

  def put10And20[U <: Union](implicit e: Member[StateInt, U]): Free[U, (Int, Int)] =
    for {
      _ <- State.put(10)
      x <- State.get
      _ <- State.put(20)
      y <- State.get
    } yield (x, y)

  test("state") {
    assert(Free.run(State.run(put10And20[StateInt :+: Void], 0)) == (20, (10, 20)))
  }

  type ChoiceVector[A] = Choice[Vector, A]

  def even[U <: Union](implicit c: Member[ChoiceVector, U]): Free[U, Int] = {
    import MonadPlus.Ops
    type F[A] = Free[U, A]
    for {
      n <- Traverse.foldMap((1 to 10).toVector)(n => Pure(n): F[Int])
      if n % 2 == 0
    } yield n
  }

  test("choice") {
    assert(Free.run(Choice.run(even[ChoiceVector :+: Void])) == Vector(2, 4, 6, 8, 10))
  }

}
