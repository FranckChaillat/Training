package booleanordertest

import booleanorder.BooleanOrder
import org.scalatest.FlatSpec

import scala.util.Success

class BooleanOrderTest extends FlatSpec {

  it should "return failure because boolean values are not corrects" in {
    val res = BooleanOrder.evaluate("tfm", "|&")
    assert(res.isFailure)
  }

  it should "return 1 when given tt and &" in {
    val res = BooleanOrder.evaluate("tt", "&")
    res match {
      case Success(1) => succeed
      case _ => fail()
    }
  }

}
