package kits.eff

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

sealed abstract class Task extends Product with Serializable

object Task {
  def context: Eff[Task, ExecutionContext] = Eff(GetContext())

  def context_=(ec: ExecutionContext): Eff[Task, Unit] = Eff(SetContext(ec))

  def lift[A](future: => Future[A]): Eff[Task, A] = {
    Eff(Lift(() => future))
  }

  def async[A](a: => A): Eff[Task, A] =
    context.flatMap { implicit ec =>
      lift(Future(a))
    }

  def run[A](eff: Eff[Task, A])(implicit ec: ExecutionContext): Future[A] = {
    def go(eff: Eff[Task, A])(implicit ec: ExecutionContext): Future[A] = loop(eff)
    @tailrec
    def loop(eff: Eff[Task, A])(implicit ec: ExecutionContext): Future[A] =
      (eff: @unchecked) match {
        case Eff.Pure(a) => Future.successful(a)
        case Eff.Impure(GetContext(), k) => loop(k(ec))
        case Eff.Impure(SetContext(ec), k) => loop(k(()))(ec)
        case Eff.Impure(Lift(f), k) => f().flatMap(a => go(k(a)))
      }
    loop(eff)
  }

  case class GetContext() extends Task

  case class SetContext(context: ExecutionContext) extends Task

  case class Lift[A](future: () => Future[A]) extends Task
}
