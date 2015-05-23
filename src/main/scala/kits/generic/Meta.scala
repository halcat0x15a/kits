package kits.generic

case class Meta[T, A](value: A)(implicit val tag: scala.reflect.ClassTag[T])
