package foo

import eu.timepit.refined.auto.*
import eu.timepit.refined.api.*
import eu.timepit.refined.types.all.*
import eu.timepit.refined.numeric.*


object Main {
  val a: NonNegInt = autoRefineV(3)
}