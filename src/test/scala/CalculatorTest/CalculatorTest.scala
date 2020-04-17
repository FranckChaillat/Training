package CalculatorTest

import calculator.Calculator
import org.scalatest.FlatSpec

class CalculatorTest extends FlatSpec {
//
//  it should "evaluate addition of 2 and 2" in {
//    val res = Calculator.evaluate("2+2")
//    assert(res == 4)
//  }
//
//  it should "evaluate multiplication of 6 and 9" in {
//    assert(Calculator.evaluate("6*9") == 54)
//  }

  it should "evaluate division of 4 and 2" in {
    assert(Calculator.evaluate("4/2") == 2)
  }
//
//  it should "evaluate minus of 9 and 4" in {
//    assert(Calculator.evaluate("9-4") == 5)
//  }
//
//  it should "evaluate 6*9+2" in {
//    assert(Calculator.evaluate("6*9+2") == 56)
//  }
//
//  it should "evaluate -4*2" in {
//    assert(Calculator.evaluate("-4*2") == -8)
//  }
//
//  it should "evaluate 6/2-7*2" in {
//    assert(Calculator.evaluate("6/2-7*2") == -11)
//  }
}
