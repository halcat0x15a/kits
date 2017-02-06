package kits

import org.mockito.Mockito
import org.mockito.stubbing.OngoingStubbing
import scala.language.experimental.macros
import scala.reflect.ClassTag

package object mock {

  def mock[A](implicit classTag: ClassTag[A]): A = Mockito.mock(classTag.runtimeClass).asInstanceOf[A]

  def when[A](call: A): MethodCall[A] = macro kits.mock.MockMacros.when[A]

  implicit class RichMock[A](val self: A) extends AnyVal {
    def returns(value: A): MethodCall[A] = macro kits.mock.MockMacros.returns[A]
    def overrides[AA >: A](impl: AA): A = macro kits.mock.MockMacros.overrides[A]
  }

}
