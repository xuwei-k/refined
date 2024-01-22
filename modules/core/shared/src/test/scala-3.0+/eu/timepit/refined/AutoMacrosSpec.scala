package eu.timepit.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.char.{Digit, Letter}
import eu.timepit.refined.generic._
import eu.timepit.refined.types.numeric.PosInt
import org.scalacheck.Prop._
import org.scalacheck.Properties

class AutoMacrosSpec extends Properties("auto.macros") {
  def illTyped(s: String, x: String): Unit = ()




  property("#260") = secure {
    val somePosInt: Option[PosInt] = Some(5)
    somePosInt.isDefined
  }
}
