package googlecodejamtests

import googlecodeJam.Incidium
import org.scalatest.FlatSpec

class IncidiumTest extends FlatSpec {

  "getTrace" should "return 1,2,3,4 for input (10, 4)" in {
    assert(Incidium.getTrace(10, 4) == List(1,2,3,4))
  }

  "getTrace" should "return 2,2,2,2 for input (4, 4)" in {
    assert(Incidium.getTrace(8, 4) == List(2,2,2,2))
  }

  "getTrace" should "return 2,2,2 for input (6, 3)" in {
    assert(Incidium.getTrace(10, 4) == List(1,2,3,4))
  }

  "getTrace" should "return nothing for input (2, 3)" in {
    assert(Incidium.getTrace(2, 3) == Seq())
  }

  "getTrace" should "return nothing for input (17, 4)" in {
    assert(Incidium.getTrace(17, 4) == Seq())
  }

}
