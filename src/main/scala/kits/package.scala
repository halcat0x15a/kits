package object kits {

  case class Identity[A](value: A) {

    def map[B](f: A => B): Identity[B] = Identity(f(value))

    def flatMap[B](f: A => Identity[B]): Identity[B] = f(value)

    def traverse[F[_], B](f: A => F[B])(implicit F: Functor[F]): F[Identity[B]] = F.map(f(value))(Identity(_))

  }

}
