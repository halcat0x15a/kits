package kits

package free

sealed abstract class Free[U, A] extends Product with Serializable {

  def map[B](f: A => B): Free[U, B]

  def flatMap[B](f: A => Free[U, B]): Free[U, B]

  def withFilter(p: A => Boolean)(implicit F: MonadPlus[({ type F[A] = Free[U, A] })#F]): Free[U, A] =
    flatMap(a => if (p(a)) Pure(a) else F.zero)

}

case class Pure[U, A](value: A) extends Free[U, A] {

  def map[B](f: A => B): Free[U, B] = Pure(f(value))

  def flatMap[B](f: A => Free[U, B]): Free[U, B] = f(value)

}

case class Impure[U, A, B](union: U, arrs: Arrows[U, A, B]) extends Free[U, B] {

  def map[C](f: B => C): Free[U, C] = Impure(union, arrs :+ (x => Pure(f(x))))

  def flatMap[C](f: B => Free[U, C]): Free[U, C] = Impure(union, arrs :+ f)

}

object Free {

  def apply[U, A](union: U): Free[U, A] = Impure(union, Arrows.singleton(Pure(_: A)))

  def run[A](free: Free[Void, A]): A =
    (free: @unchecked) match {
      case Pure(a) => a
    }

  def handleRelay[F, U, A, B](free: Free[F :+: U, A])(
    f: A => Either[Free[F :+: U, A], B],
    g: (F, Any => Free[F :+: U, A]) => Either[Free[F :+: U, A], Free[U, B]]
  ): Free[U, B] =
    free match {
      case Pure(a) =>
        f(a) match {
          case Right(b) => Pure(b)
          case Left(free) => handleRelay(free)(f, g)
        }
      case Impure(Inl(fa), k) =>
        g(fa, k.apply) match {
          case Right(free) => free
          case Left(free) => handleRelay(free)(f, g)
        }
      case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => handleRelay(k(x))(f, g)))
    }

  def handleRelayS[F, U, A, B, S](free: Free[F :+: U, A], state: S)(
    f: (A, S) => Either[(Free[F :+: U, A], S), B],
    g: (F, S, Any => Free[F :+: U, A]) => Either[(Free[F :+: U, A], S), Free[U, B]]
  ): Free[U, B] =
    free match {
      case Pure(a) =>
        f(a, state) match {
          case Right(b) => Pure(b)
          case Left((free, state)) => handleRelayS(free, state)(f, g)
        }
      case Impure(Inl(fa), k) =>
        g(fa, state, k.apply) match {
          case Right(free) => free
          case Left((free, state)) => handleRelayS(free, state)(f, g)
        }
      case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => handleRelayS(k(x), state)(f, g)))
    }

  def interpose[F, U, A, B](free: Free[U, A])(
    f: A => Either[Free[U, A], B],
    g: (F, Any => Free[U, A]) => Either[Free[U, A], Free[U, B]]
  )(implicit F: Member[F, U]): Free[U, B] =
    free match {
      case Pure(a) =>
        f(a) match {
          case Right(b) => Pure(b)
          case Left(free) => interpose(free)(f, g)
        }
      case Impure(u, k) =>
        F.project(u) match {
          case Some(fa) =>
            g(fa, k.apply) match {
              case Right(free) => free
              case Left(free) => interpose(free)(f, g)
            }
          case None => Impure(u, Arrows.singleton((x: Any) => interpose(k(x))(f, g)))
        }
    }

  def interposeS[F, U, A, B, S](free: Free[U, A], state: S)(
    f: (A, S) => Either[(Free[U, A], S), B],
    g: (F, S, Any => Free[U, A]) => Either[(Free[U, A], S), Free[U, B]]
  )(implicit F: Member[F, U]): Free[U, B] =
    free match {
      case Pure(a) =>
        f(a, state) match {
          case Right(b) => Pure(b)
          case Left((free, state)) => interposeS(free, state)(f, g)
        }
      case Impure(u, k) =>
        F.project(u) match {
          case Some(fa) =>
            g(fa, state, k.apply) match {
              case Right(free) => free
              case Left((free, state)) => interposeS(free, state)(f, g)
            }
          case None => Impure(u, Arrows.singleton((x: Any) => interposeS(k(x), state)(f, g)))
        }
    }

  trait FreeMonad[U] extends Monad[({ type F[A] = Free[U, A] })#F] {
    final def pure[A](a: A): Free[U, A] = Pure(a)
    final override def map[A, B](fa: Free[U, A])(f: A => B): Free[U, B] = fa.map(f)
    final def flatMap[A, B](fa: Free[U, A])(f: A => Free[U, B]): Free[U, B] = fa.flatMap(f)
  }

  implicit def FreeMonad[U]: FreeMonad[U] = new FreeMonad[U] {}

  implicit def ChoiceMonadPlus[U: Choice#Member]: MonadPlus[({ type F[A] = Free[U, A] })#F] =
    new MonadPlus[({ type F[A] = Free[U, A] })#F] with FreeMonad[U] {
      def zero[A]: Free[U, A] = Choice.zero
      def plus[A](x: Free[U, A], y: Free[U, A]): Free[U, A] = Choice.plus(x, y)
    }

  implicit def ChoiceMonoid[U: Choice#Member, A]: Monoid[Free[U, A]] = ChoiceMonadPlus[U].monoid

  implicit def MaybeMonadPlus[U: Maybe#Member]: MonadPlus[({ type F[A] = Free[U, A] })#F] =
    new MonadPlus[({ type F[A] = Free[U, A] })#F] with FreeMonad[U] {
      def zero[A]: Free[U, A] = Maybe.nothing
      def plus[A](x: Free[U, A], y: Free[U, A]): Free[U, A] = Maybe.orElse(x, y)
    }

  implicit def MaybeMonoid[U: Maybe#Member, A]: Monoid[Free[U, A]] = MaybeMonadPlus[U].monoid

}
