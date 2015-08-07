package object kits {

  case class Identity[A](value: A) {

    def map[B](f: A => B): Identity[B] = Identity(f(value))

    def flatMap[B](f: A => Identity[B]): Identity[B] = f(value)

    def traverse[F[_], B](f: A => F[B])(implicit F: Functor[F]): F[Identity[B]] = F.map(f(value))(Identity(_))

  }

  case class Sum[A](value: A) extends AnyVal {

    def append(that: Sum[A])(implicit A: Numeric[A]): Sum[A] = Sum(A.plus(value, that.value))

  }

  case class Product[A](value: A) extends AnyVal {

    def append(that: Product[A])(implicit A: Numeric[A]): Product[A] = Product(A.times(value, that.value))

  }

  case class All(value: Boolean) extends AnyVal {

    def append(that: All): All = All(value && that.value)

  }

  case class Any(value: Boolean) extends AnyVal {

    def append(that: Any): Any = Any(value || that.value)

  }

  case class First[A](value: Option[A]) extends AnyVal {

    def append(that: First[A]): First[A] = First(value.orElse(that.value))

  }

  case class Last[A](value: Option[A]) extends AnyVal {

    def append(that: Last[A]): Last[A] = Last(that.value.orElse(value))

  }

  case class Endo[A](value: A => A) extends AnyVal {

    def append(that: Endo[A]): Endo[A] = Endo(value.andThen(that.value))

  }

  case class Validation[E, A](value: Either[E, A]) extends AnyVal {

    def map[B](f: A => B): Validation[E, B] = Validation(value.right.map(f))

    def ap[B](f: Validation[E, A => B])(implicit E: Monoid[E]): Validation[E, B] =
      (f.value, value) match {
        case (Right(f), Right(a)) => Validation(Right(f(a)))
        case (Right(_), Left(e)) => Validation(Left(e))
        case (Left(e), Right(_)) => Validation(Left(e))
        case (Left(x), Left(y)) => Validation(Left(E.append(x, y)))
      }

    def traverse[F[_], B](f: A => F[B])(implicit F: Applicative[F]): F[Validation[E, B]] =
      value match {
        case Left(e) => F.pure(Validation(Left(e)))
        case Right(a) => F.map(f(a))(b => Validation(Right(b)))
      }

  }

  case class Reader[F[_], R, A](value: R => F[A]) extends AnyVal {

    def map[B](f: A => B)(implicit F: Functor[F]): Reader[F, R, B] =
      Reader(r => F.map(value(r))(f))

    def ap[B](f: Reader[F, R, A => B])(implicit F: Applicative[F]): Reader[F, R, B] =
      Reader(r => F.ap(value(r))(f.value(r)))

    def flatMap[B](f: A => Reader[F, R, B])(implicit F: Monad[F]): Reader[F, R, B] =
      Reader(r => F.flatMap(value(r))(a => f(a).value(r)))

  }

  case class Writer[F[_], W, A](value: F[(W, A)]) extends AnyVal {

    def map[B](f: A => B)(implicit F: Functor[F]): Writer[F, W, B] =
      Writer(F.map(value) { case (w, a) => (w, f(a)) })

    def ap[B](f: Writer[F, W, A => B])(implicit F: Applicative[F], W: Monoid[W]): Writer[F, W, B] =
      Writer(F.map(f.value, value) { case ((x, f), (y, a)) => (W.append(x, y), f(a)) })

    def flatMap[B](f: A => Writer[F, W, B])(implicit F: Monad[F], W: Monoid[W]): Writer[F, W, B] =
      Writer(F.flatMap(value) { case (x, a) => F.map(f(a).value) { case (y, b) => (W.append(x, y), b) } })

    def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[Writer[F, W, B]] =
      G.map(F.traverse(value) { case (w, a) => G.map(f(a))(b => (w, b)) })(Writer(_))

  }

  case class State[F[_], S, A](value: S => F[(S, A)]) extends AnyVal {

    def map[B](f: A => B)(implicit F: Functor[F]): State[F, S, B] =
      State(s => F.map(value(s)) { case (s, a) => (s, f(a)) })

    def flatMap[B](f: A => State[F, S, B])(implicit F: Monad[F]): State[F, S, B] =
      State(s => F.flatMap(value(s)) { case (s, a) => f(a).value(s) })

  }

}
