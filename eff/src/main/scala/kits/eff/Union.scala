package kits.eff

import scala.reflect.Manifest

case class Union[-A] private (tag: String, value: Any) {
  def decomp[F, R](implicit F: Manifest[F], ev: ~[F] with R <:< A): Either[F, Union[R]] =
    if (F.toString == tag)
      Left(value.asInstanceOf[F])
    else
      Right(this.asInstanceOf[Union[R]])
}

object Union {
  def apply[A](a: A)(implicit A: Manifest[A]): Union[~[A]] = new Union(A.toString, a)
}
