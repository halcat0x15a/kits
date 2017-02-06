package kits.mock

import org.mockito.Mockito
import org.mockito.stubbing.OngoingStubbing
import org.mockito.verification.VerificationMode

trait MethodCall[A] { self =>

  def stubbing: OngoingStubbing[A]

  def verify(mode: VerificationMode): A

  def verify(): A = verify(Mockito.times(1))

  def returns(value: A): MethodCall[A] =
    new MethodCall[A] {
      val stubbing = self.stubbing.thenReturn(value)
      def verify(mode: VerificationMode) = self.verify(mode)
    }

}
