package googlecodejam

import googlecodeJam.Incidium
import googlecodeJam.Incidium.Matrix
import org.scalatest.FlatSpec

class IncidiumTest extends FlatSpec {

  private def assertMatrixRow(array: Matrix, expectedLen: Int): Boolean = {
    array.length == expectedLen &&
    array.forall(row => row.length == expectedLen) &&
    array.indices.forall(i => array(i).groupBy(n => n).forall(g => g._2.length == 1))
  }

  "getTrace" should "return List(4,4,3,2) for input 13, 4" in {
    val res = Incidium.getTrace(13, 4)
    assert(res == List(4,4,3,2))
  }

  "getTrace" should "return List(4,1,1,1) for input 7, 4" in {
    val res = Incidium.getTrace(7, 4)
    assert(res == List(4,1,1,1))
  }

  "getTrace" should "return List(1,1,1,1) for input 4, 4" in {
    val res = Incidium.getTrace(4, 4)
    assert(res == List(1,1,1,1))
  }

  "getTrace" should "return List(4,4,4,4) for input 16,4" in {
    val res = Incidium.getTrace(16,4)
    assert(res == List(4,4,4,4))
  }

  "getTrace" should "return List() for input 15, 4" in {
    val res = Incidium.getTrace(15, 4)
    assert(res.isEmpty)
  }

  "fillMatrix" should "return good result for trace 4, 4, 4, 4" in {
    val res = Incidium.fillMatrix(Seq(4, 4, 4, 4))
    assert(assertMatrixRow(res, 4))
  }

}
