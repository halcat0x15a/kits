package kits.mock

import org.mockito.Matchers
import org.mockito.Mockito
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.mockito.stubbing.OngoingStubbing
import org.mockito.verification.VerificationMode
import scala.reflect.macros.blackbox.Context

class MockMacros(val c: Context) {

  import c.universe._

  def when[A: WeakTypeTag](call: c.Tree): c.Expr[MethodCall[A]] = {
    val q"$expr.$tname(...$exprss)" = call
    val A = symbolOf[A]
    val MethodCall = symbolOf[MethodCall[_]]
    val Mockito = symbolOf[Mockito].companion
    val OngoingStubbing = symbolOf[OngoingStubbing[_]]
    val VerificationMode = symbolOf[VerificationMode]
    val methodCall = q"""
      new $MethodCall[$A] {
        val stubbing: $OngoingStubbing[$A] = $Mockito.when($call)
        def verify(mode: $VerificationMode): $A = $Mockito.verify($expr, mode).$tname(...$exprss)
      }
    """
    c.Expr[MethodCall[A]](methodCall)
  }

  def returns[A: WeakTypeTag](value: c.Tree): c.Expr[MethodCall[A]] = {
    val q"$_($self)" = c.prefix.tree
    val call = when[A](self).tree
    c.Expr[MethodCall[A]](q"$call.returns($value)")
  }

  def overrides[A](impl: Tree): c.Expr[A] = {
    val q"$_($self)" = c.prefix.tree
    val Mockito = symbolOf[Mockito].companion
    val Matchers = symbolOf[Matchers].companion
    val Answer = symbolOf[Answer[_]]
    val InvocationOnMock = symbolOf[InvocationOnMock]
    val q"new { ..$_ } with ..$_ { $_ => ..$stats }" = impl
    val exprs = stats.map {
      case q"$_ def $tname[..$tparams](...$paramss): $tpt = $expr" =>
        val argss = paramss.map(_.map {
          case q"$_ val $_: $tpt" => q"$Matchers.any[$tpt]"
        })
        val decls = paramss.flatten.zipWithIndex.map {
          case (q"$_ val $tname: $tpt", i) =>
            q"val $tname: $tpt = invocation.getArgumentAt($i, classOf[$tpt])"
        }
        val body = c.parse(show(expr))
        val answer = q"new $Answer[$tpt] { def answer(invocation: $InvocationOnMock): $tpt = { ..$decls; $body } }"
        q"$Mockito.when($self.$tname[..$tparams](...$argss)).then($answer)"
      case q"$_ val $tname: $_ = $expr" =>
        q"$Mockito.when($self.$tname).thenReturn($expr)"
      case _ => q""
    }
    c.Expr(q"{ ..$exprs; $self }")
  }

}
