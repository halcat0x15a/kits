package kits.eff

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

sealed abstract class Task extends Product with Serializable

object Task {
  def context: Eff[~[Task], ExecutionContext] = Eff(Context(): Task)

  def lift[A](future: Future[A]): Eff[~[Task], A] = Eff(Lift(future): Task)

  def async[A](a: => A): Eff[~[Task], A] =
    context.flatMap { implicit ec =>
      lift(Future(a))
    }

  def run[A](eff: Eff[~[Task], A])(implicit ec: ExecutionContext): Future[A] =
    eff match {
      case Eff.Pure(a) => Future.successful(a)
      case Eff.Impure(Union.Tagless(a), k) => run(k(a))
      case Eff.Impure(Union.Tagged(_, Context()), k) => run(k(ec))
      case Eff.Impure(Union.Tagged(_, Lift(f)), k) => f.flatMap(a => run(k(a)))
    }

  case class Context() extends Task

  case class Lift[A](future: Future[A]) extends Task
}
