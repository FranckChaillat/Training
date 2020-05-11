package googlecodejam

import googlecodeJam.Incidium
import googlecodeJam.Incidium.{Mask, Matrix}
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

  "getMask" should "return a complete mask for 4,4,4,4" in {
    val res = Incidium.getMasks(Seq(4,4,4,4))
    assert(res.length == 4)
    assert(res.forall(p => p == Set(1,2,3)))
  }

  "getMask" should "return a complete mask for 1,2,3,4" in {
    val res = Incidium.getMasks(Seq(1,2,3,4))
    assert(res.length == 4)
    val expectedMask = Array(
      Set(2,3,4),
      Set(1,3,4),
      Set(1,2,4),
      Set(1,2,3)
    )
    assert(res sameElements expectedMask)
  }

  "getDispatch" should "work with mask  1,2,3" in {
    val rowMask : Mask = Array(Set(1,2,3))
    val colMask : Mask = Array(Set(1,2,3), Set(1,2,3), Set(1,2,3), Set(1,2,3))
    val res = Incidium.getDispatch(rowMask, colMask, 0)
    res.forall(_ == (1, Set(1,2,3)))
  }

  "getDispatch" should "work with matrix with trace = 1,2,3,4" in {
    val rowMask : Mask = Array(Set(2,3,4), Set(1,3,4), Set(1,2,4), Set(1,2,3))
    val colMask : Mask = rowMask
    val resRow1 = Incidium.getDispatch(rowMask, colMask, 0)

    assert(resRow1.head._1 == 2 && resRow1.head._2 == Set(2, 3))
    assert(resRow1(1)._1 == 3 && resRow1(1)._2 == Set(1, 3))
    assert(resRow1(2)._1 == 4 && resRow1(2)._2 == Set(1, 2))
  }

  "fillMatrix" should "return good result for trace 4, 4, 4, 4" in {
    val res = Incidium.fillMatrix(Seq(4, 4, 4, 4))
    assert(assertMatrixRow(res, 4))
    assert(res(0) sameElements Array(4, 1, 2, 3))
    assert(res(1) sameElements Array(2, 4, 3, 1))
    assert(res(2) sameElements Array(1, 3, 4, 2))
    assert(res(3) sameElements Array(3, 2, 1, 4))
  }

  "fillMatrix" should "return good result for trace 1,2,3,4" in {
    val res = Incidium.fillMatrix(Seq(1, 2, 3, 4))
    assert(assertMatrixRow(res, 4))
    assert(res(0) sameElements Array(1, 4, 2, 3))
    assert(res(1) sameElements Array(3, 2, 4, 1))
    assert(res(2) sameElements Array(4, 1, 3, 2))
    assert(res(3) sameElements Array(2, 3, 1, 4))
  }

  "fillMatrix" should "return good result for trace 4,4,3,2" in {
    val res = Incidium.fillMatrix(Seq(4, 4, 3, 2))
    assert(assertMatrixRow(res, 4))
    assert(res(0) sameElements Array(4, 2, 1, 3))
    assert(res(1) sameElements Array(3, 4, 2, 1))
    assert(res(2) sameElements Array(2, 1, 3, 4))
    assert(res(3) sameElements Array(1, 3, 4, 2))
  }


  "fillMatrix" should "stress test" in {
    val res = Incidium.fillMatrix(Seq.fill(100)(1))
    assert(assertMatrixRow(res, 10))

  }

}
