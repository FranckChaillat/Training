package booleanorder

import scala.util.{Failure, Try}

object BooleanOrder {

  private type BooleanOperation = (Boolean, Boolean) => Boolean
  private val operations = Map[Char, BooleanOperation]('&' -> ((a: Boolean,b: Boolean) => a & b),
                               '|' -> ((a: Boolean,b: Boolean) => a | b),
                               '^' -> ((a: Boolean,b: Boolean) => a ^ b))

  def evaluate(booleanValues: String, operators: String): Try[Int] = {
    if(booleanValues.forall(v => v == 't' || v == 'f')
    && operators.forall(o => o == '|' || o == '&' || o == '^')
    && operators.length == booleanValues.length - 1) {
      ???
    } else {
      Failure(new IllegalArgumentException("given operators or boolean values are not corrects."))
    }
  }

  private def getEvaluation(booleans: Seq[Boolean], operations: Seq[BooleanOperation]) = {


//    operations.headOption match {
//      case Some(operation) =>
//        booleans match {
//          case v1 +: v2 +: tail => operation(v1, v2)
//        }
//    }
  }
}
