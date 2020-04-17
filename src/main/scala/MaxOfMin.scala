import scala.annotation.tailrec

//object MaxOfMin extends App {
//
//  def getMaxOfMin(elements: Array[Int]) = {
//    (for(windowSize <- (1 to elements.length).reverse)
//      yield compute(elements, windowSize)).toArray
//  }
//
//  private def compute(elements: Array[Int], windowSize: Int)= {
//    val windowedMin = for(i <- 0.to(elements.length - windowSize))
//      yield elements.slice(i, i + windowSize).min
//    windowedMin.max
//  }
//
//  val res = getMaxOfMin(Array(10, 20, 30, 50, 10, 70, 30))
//  print(res.mkString(","))
//
//}

case class State(value: Int, index: Int)

object MaxOfMin extends App {

  def tryGetFromState(state: Array[State], range: Range): Option[Int] = {
    state.find(s => {
      range.contains(s.value)
    }).map(_.value)
  }

  @tailrec
  def getWindowedMin(elements: Array[Int], windowSize: Int, index: Int, state: Array[State], acc: Array[Int]): (Array[State], Int) = {
    if (index + windowSize > elements.length) {
      (state, acc.max)
    } else {
      val (start, end) = (index, index + windowSize)
      tryGetFromState(state, start to end) match {
        case Some(value) =>
          getWindowedMin(elements, windowSize, index + 1, state, acc.:+(value))
        case None =>
          val value = elements.slice(index, index + windowSize).min
          val newState = state.:+(State(value, value))
          getWindowedMin(elements, windowSize, index + 1, newState, acc.:+(value))
      }
    }
  }

  def getMaxOfMin(elements: Array[Int]) = {
    @tailrec
    def go(state: Array[State], acc: Array[Int], windowSize: Int): Array[Int] = {
      if(windowSize == 0)
        acc
      else {
        val (newState, min) = getWindowedMin(elements, windowSize, 0, state, Array())
        go(newState, acc.:+(min), windowSize - 1)
      }
    }
  }

}