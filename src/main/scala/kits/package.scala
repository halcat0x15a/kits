package object kits {

  trait Ops[A] extends Any {

    def self: A { type Tag }

    def value: A = self.asInstanceOf[A]

  }

  type Identity[A] = A { type Tag = IdentityOps[A] }

  def Identity[A](value: A): Identity[A] = value.asInstanceOf[Identity[A]]

  implicit class IdentityOps[A](val self: Identity[A]) extends AnyVal with Ops[A] {

    def map[B](f: A => B): Identity[B] = Identity(f(self))

    def ap[B](f: Identity[A => B]): Identity[B] = Identity(f(self))

    def flatMap[B](f: A => Identity[B]): Identity[B] = f(self)

    def traverse[F[_], B](f: A => F[B])(implicit F: Functor[F]): F[Identity[B]] = F.map(f(self))(Identity)

  }

  type Sum[A] = A { type Tag = SumOps[A] }

  def Sum[A](value: A): Sum[A] = value.asInstanceOf[Sum[A]]

  implicit class SumOps[A](val self: Sum[A]) extends AnyVal with Ops[A] {

    def append(that: Sum[A])(implicit A: Numeric[A]): Sum[A] = Sum(A.plus(self, that))

  }

  type Prod[A] = A { type Tag = ProdOps[A] }

  def Prod[A](value: A): Prod[A] = value.asInstanceOf[Prod[A]]

  implicit class ProdOps[A](val self: Prod[A]) extends AnyVal with Ops[A] {

    def append(that: Prod[A])(implicit A: Numeric[A]): Prod[A] = Prod(A.times(self, that))

  }

  type Conj = Boolean { type Tag = ConjOps }

  def Conj(value: Boolean): Conj = value.asInstanceOf[Conj]

  implicit class ConjOps(val self: Conj) extends AnyVal with Ops[Boolean] {

    def append(that: Conj): Conj = Conj(self && that)

  }

  type Disj = Boolean { type Tag = DisjOps }

  def Disj(value: Boolean): Disj = value.asInstanceOf[Disj]

  implicit class DisjOps(val self: Disj) extends AnyVal with Ops[Boolean] {

    def append(that: Disj): Disj = Disj(self || that)

  }

  type First[A] = Option[A] { type Tag = FirstOps[A] }

  def First[A](value: Option[A]): First[A] = value.asInstanceOf[First[A]]

  implicit class FirstOps[A](val self: First[A]) extends AnyVal with Ops[Option[A]] {

    def append(that: First[A]): First[A] = First(self.orElse(that))

  }

  type Last[A] = Option[A] { type Tag = LastOps[A] }

  def Last[A](value: Option[A]): Last[A] = value.asInstanceOf[Last[A]]

  implicit class LastOps[A](val self: Last[A]) extends AnyVal with Ops[Option[A]] {

    def append(that: Last[A]): Last[A] = Last(that.orElse(self))

  }

  type Endo[A] = (A => A) { type Tag = EndoOps[A] }

  def Endo[A](value: A => A): Endo[A] = value.asInstanceOf[Endo[A]]

  implicit class EndoOps[A](val self: Endo[A]) extends AnyVal with Ops[A => A] {

    def append(that: Endo[A]): Endo[A] = Endo(self.andThen(that))

  }

  type Validation[E, A] = Either[E, A] { type Tag = ValidationOps[E, A] }

  def Validation[E, A](value: Either[E, A]): Validation[E, A] = value.asInstanceOf[Validation[E, A]]

  implicit class ValidationOps[E, A](val self: Validation[E, A]) extends AnyVal with Ops[Either[E, A]] {

    def map[B](f: A => B): Validation[E, B] =
      value match {
        case Right(a) => Validation(Right(f(a)))
        case Left(e) => Validation(Left(e))
      }

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

  type Reader[F[_], R, A] = (R => F[A]) { type Tag = ReaderOps[F, R, A] }

  def Reader[F[_], R, A](value: R => F[A]): Reader[F, R, A] = value.asInstanceOf[Reader[F, R, A]]

  implicit class ReaderOps[F[_], R, A](val self: Reader[F, R, A]) extends AnyVal with Ops[R => F[A]] {

    def map[B](f: A => B)(implicit F: Functor[F]): Reader[F, R, B] =
      Reader(r => F.map(self(r))(f))

    def ap[B](f: Reader[F, R, A => B])(implicit F: Applicative[F]): Reader[F, R, B] =
      Reader(r => F.ap(self(r))(f(r)))

    def flatMap[B](f: A => Reader[F, R, B])(implicit F: Monad[F]): Reader[F, R, B] =
      Reader(r => F.flatMap(self(r))(a => f(a)(r)))

  }

  type Writer[F[_], W, A] = F[(W, A)] { type Tag = WriterOps[F, W, A] }

  def Writer[F[_], W, A](value: F[(W, A)]): Writer[F, W, A] = value.asInstanceOf[Writer[F, W, A]]

  implicit class WriterOps[F[_], W, A](val self: Writer[F, W, A]) extends AnyVal with Ops[F[(W, A)]] {

    def map[B](f: A => B)(implicit F: Functor[F]): Writer[F, W, B] =
      Writer(F.map(self) { case (w, a) => (w, f(a)) })

    def ap[B](f: Writer[F, W, A => B])(implicit F: Applicative[F], W: Monoid[W]): Writer[F, W, B] =
      Writer(F.map(f, self) { case ((x, f), (y, a)) => (W.append(x, y), f(a)) })

    def flatMap[B](f: A => Writer[F, W, B])(implicit F: Monad[F], W: Monoid[W]): Writer[F, W, B] =
      Writer(F.flatMap(self) { case (x, a) => F.map(f(a)) { case (y, b) => (W.append(x, y), b) } })

    def traverse[G[_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[Writer[F, W, B]] =
      G.map(F.traverse(self) { case (w, a) => G.map(f(a))(b => (w, b)) })(Writer(_))

  }

  type State[F[_], S, A] = (S => F[(S, A)]) { type Tag = StateOps[F, S, A] }

  def State[F[_], S, A](value: S => F[(S, A)]): State[F, S, A] = value.asInstanceOf[State[F, S, A]]

  implicit class StateOps[F[_], S, A](val self: State[F, S, A]) extends AnyVal with Ops[S => F[(S, A)]] {

    def map[B](f: A => B)(implicit F: Functor[F]): State[F, S, B] =
      State(s => F.map(self(s)) { case (s, a) => (s, f(a)) })

    def flatMap[B](f: A => State[F, S, B])(implicit F: Monad[F]): State[F, S, B] =
      State(s => F.flatMap(self(s)) { case (s, a) => f(a)(s) })

  }

}
