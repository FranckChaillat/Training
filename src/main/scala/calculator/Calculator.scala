package calculator

import scala.util.matching.Regex


object Calculator {

  private type Operation = (Int, Int) => Int

  private val operations = Map[String, Operation](
    "+" -> ((a: Int,b: Int) => a + b),
    "-" -> ((a: Int,b: Int) => a - b),
    "*" -> ((a: Int,b: Int) => a * b),
    "/" -> ((a: Int, b: Int) => a/b))

  def evaluate(operation: String) : Int = {
    def regex(op:String): Regex = {
      val s = "((\\d+)(\\" + op + ")(\\d+)).*"
      s.r.unanchored
    }

    val mult = regex("*")
    val div = regex("/")
    val add = regex("+")
    val minus = regex("-")

    def go(operation: String): Int = {
      operation match {
        case mult(whole, left, op, right) =>
          val res = applyEvaluation(left, op, right)
          go(operation.replace(whole, res))
        case div(whole, left, op, right) =>
          val res = applyEvaluation(left, op, right)
          go(operation.replace(whole, res))
        case add(whole, left, op, right) =>
          val res = applyEvaluation(left, op, right)
          go(operation.replace(whole, res))
        case minus(whole, left, op, right) =>
          val res = applyEvaluation(left, op, right)
          go(operation.replace(whole, res))
        case _ =>
          operation.toInt
      }
    }
    go(operation.trim)
  }


  private def applyEvaluation(left: String, operator: String, right: String) = {
    operations(operator)(left.toInt, right.toInt).toString
  }
}
