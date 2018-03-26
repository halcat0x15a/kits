package kits.eff

import org.scalatest.AsyncFlatSpec

class TaskSpec extends AsyncFlatSpec {
  "Task" should "run as Future asynchronously" in {
    val e = for {
      n <- Reader.ask[Int]
      _ <- Writer.tell(n)
      m <- Task.async(n * n)
    } yield m
    Task.run(Reader.run(2)(Writer.runVector(e))).map { result =>
      assert(result == (Vector(2), 4))
    }
  }
}
