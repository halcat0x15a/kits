package kits.eff

import org.scalatest.AsyncFlatSpec
import scala.concurrent.ExecutionContext

class TaskSpec extends AsyncFlatSpec {
  "Task" should "run as Future asynchronously" in {
    val e = for {
      n <- Reader.ask[Int]
      _ <- Writer.tell(n)
      m <- Task.async(n * n)
    } yield m
    Task.run(Reader.run(2)(Writer.run(e))).map { result =>
      assert(result == (Vector(2), 4))
    }
  }

  it should "set and get the context" in {
    val e = for {
      _ <- Task.context = ExecutionContext.global
      ec <- Task.context
    } yield ec
    Task.run(e).map { result =>
      assert(result == ExecutionContext.global)
    }
  }
}
