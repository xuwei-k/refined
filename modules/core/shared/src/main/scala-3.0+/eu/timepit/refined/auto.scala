package eu.timepit.refined

import eu.timepit.refined.api.{Refined, RefType, Validate}
import eu.timepit.refined.api.Inference.==>
import eu.timepit.refined.char.{Digit, Letter, LowerCase, UpperCase, Whitespace}
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.internal.Resources
import eu.timepit.refined.numeric.{Negative, NonNegative, NonPositive, Positive}
import eu.timepit.refined.types.numeric.*
import eu.timepit.refined.generic.Equal
import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Type
import scala.quoted.FromExpr
import scala.compiletime.error

/**
 * Module that provides automatic refinements and automatic conversions
 * between refined types (refinement subtyping) at compile-time.
 */
object auto {

  /**
   * Implicitly unwraps the `T` from a value of type `F[T, P]` using the
   * `[[api.RefType]]` instance of `F`. This allows a `F[T, P]` to be
   * used as it were a subtype of `T`.
   *
   * Example: {{{
   * scala> import eu.timepit.refined.auto.autoUnwrap
   *      | import eu.timepit.refined.types.numeric.PosInt
   *
   * scala> def plusOne(i: Int): Int = i + 1
   *      | val x = PosInt.unsafeFrom(42)
   *
   * // converts x implicitly to an Int:
   * scala> plusOne(x)
   * res0: Int = 43
   * }}}
   *
   * Note: This conversion is not needed if `F[T, _] <: T` holds (which
   * is the case for `shapeless.tag.@@`, for example).
   */
  implicit def autoUnwrap[F[_, _], T, P](tp: F[T, P])(implicit rt: RefType[F]): T =
    rt.unwrap(tp)

  implicit inline def autoRefineV[T: FromExpr, P](inline t: T)(implicit
      inline rt: RefType[Refined],
      inline v: Validate[T, P]
  ): Refined[T, P] =
    ${ autoRefineVImpl[T, P]('t, 'rt, 'v) }

  private def autoRefineVImpl[T: Type, P: Type](
      t: Expr[T],
      rt: Expr[RefType[Refined]],
      v: Expr[Validate[T, P]]
  )(using q: Quotes): Expr[Refined[T, P]] = {
    import q.reflect.report
    println("hoge")

    val validate = t match {
      case '{ ${ Expr(tValue) }: Int } =>
        val validate = summon[Type[P]] match {
          case '[Positive] =>
            Validate[Int, Positive]
          case '[NonPositive] =>
            Validate[Int, NonPositive]
          case '[Negative] =>
            Validate[Int, Negative]
          case '[NonNegative] =>
            Validate[Int, NonNegative]
        }
        val res = validate.validate(tValue)
        if (res.isFailed) {
          report.errorAndAbort(validate.showResult(tValue, res))
        }
      case '{ ${ Expr(tValue) }: Char } =>
        val validate = summon[Type[P]] match {
          case '[Digit] =>
            Validate[Char, Digit]
          case '[Letter] =>
            Validate[Char, Letter]
          case '[LowerCase] =>
            Validate[Char, LowerCase]
          case '[UpperCase] =>
            Validate[Char, UpperCase]
          case '[Whitespace] =>
            Validate[Char, Whitespace]
        }
        val res = validate.validate(tValue)
        if (res.isFailed) {
          report.errorAndAbort(validate.showResult(tValue, res))
        }
      case other =>
        report.errorAndAbort(s"unsupported type ${t.show}")

      /*
      case '[Long]   =>
      case '[Double] =>
      case '[String] =>

       */
    }

    '{ Refined.unsafeApply[T, P]($t) }
  }
}
