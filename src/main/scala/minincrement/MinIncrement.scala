package minincrement

import scala.annotation.tailrec
import scala.util.Random

object MinIncrement extends App{

  def getMinCount(arr: Array[Int]): Int = {
    @tailrec
    def count(i: Int, counter: Int, currentState: Array[Int]) : Int = {
      if(i == arr.length - 1) {
        counter
      } else {
        val current = arr(i)
        val next = arr(i+1)
        if(current > next) {
          val incrementCost = getIncrSideEffect(current, arr.slice(i + 2, arr.length))
          val decrementCost = getDecrSideEffect(next, currentState)
          if(incrementCost < decrementCost){
            count(i + 1, counter + incrementCost + Math.abs(current - next), currentState.:+(current))
          } else {
            val equalized = equalize(currentState.:+(current), next)
            count(i + 1, counter + decrementCost + Math.abs(current - next), equalized.:+(current))
          }
        } else
          count(i + 1, counter, currentState.:+(arr(i)))
      }
    }
    count(0, 0, Array())
  }

  private def equalize(arr: Array[Int], value: Int) = {
    arr.map(e => if(e > value) value else e)
  }

  private def getIncrSideEffect =
    getSideEffect((_: Int) < (_: Int))_


  private def getDecrSideEffect =
    getSideEffect((_: Int) > (_: Int))_


  private def getSideEffect(operator: (Int, Int) => Boolean)(newValue: Int, arr: Array[Int]) : Int = {
    arr.foldLeft(0)((counter, e) => {
      if(operator(e,newValue))
        counter + Math.abs(newValue - e)
      else
        counter
    })
  }



  val r = new Random()
  val arr = (for(i <- 0 to(100000)) yield r.nextInt(10000)).toArray

//  utils.Utils.time {
//    println(getMinCount(arr))
//  }
  println(getMinCount(Array(1, 3, 1, 3)))

  //println(getMinCount(Array(12,4,24,42,30,28)))
  //println(getMinCount(Array(1,2,1,4,3)))
}

// 100 -> 0.06
// 1000 -> 0.10
// 2000 -> 0.15
// 10000 -> 1.22
// 100000 -> 87.8