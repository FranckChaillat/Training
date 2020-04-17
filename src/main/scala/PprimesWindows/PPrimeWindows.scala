package PprimesWindows

import utils.Utils

object PPrimeWindows {

  def getPrimes(range: Range): Set[Int] = {
    def isPrime(n: Int): Boolean = {
      if(n < 3)
        true
      else {
        val lim = math.ceil(math.sqrt(n)).toInt
        for (x <- 2 to lim) {
          if (n % x == 0) {
            return false
          }
        }
        true
      }
    }

    (for {
      i <- range
      if isPrime(i)
    } yield i).toSet
  }

  /*def countPrimeInRange(primes: Array[Int], startValue: Int, endValue: Int): Option[Int] = {
    @scala.annotation.tailrec
    def getBoundedValueIndex(lowerIndex: Option[Int], upperIndex: Option[Int], currentIndex: Int): Option[Int] = {
      if(currentIndex == primes.length - 1){
        for { l <- lowerIndex; u <- upperIndex }
          yield u - l + 1
      }

      (lowerIndex, upperIndex) match {
        case (Some(l), Some(u)) => Some(u - l + 1)
        case (None, _) =>
          val lowerBound = if(primes(currentIndex) >= startValue) Some(currentIndex) else None
          getBoundedValueIndex(lowerBound, upperIndex, currentIndex + 1)
        case (Some(l), None) =>
          val upperBound =
            if(primes(currentIndex) > endValue) Some(currentIndex - 1)
            else if(primes(currentIndex) == endValue) Some(currentIndex)
            else None
          getBoundedValueIndex(lowerIndex, upperBound, currentIndex + 1)
      }
    }
    getBoundedValueIndex(None, None, 0)
  }*/

  def evaluateWindow(primes: Array[Int], expectedNb: Int)(w: IndexedSeq[Int]) : Boolean = {
    //countPrimeInRange(primes, w.head, w.last).getOrElse(0) >= expectedNb
    w.count(x => primes.contains(x)) >= expectedNb
  }

  def getMinWindowsSize(x: Int, y: Int, p: Int) : Option[Int] = {
    val r = Range(x, y)
    val windowEvaluator = evaluateWindow(getPrimes(Range(x, y+10)), p)_

    @scala.annotation.tailrec
    def apply(windowSize: Int): Option[Int] = {
      if(windowSize == y - x)
        None
      else if(r.sliding(windowSize).toStream.par.forall { w => windowEvaluator(w)}) {
        Some(windowSize)
      } else {
        apply(windowSize + 1)
      }
    }

    apply(p)
  }

  def main(args: Array[String]): Unit = {
    val res = Utils.time {
      getMinWindowsSize(12, 100000, 20)
    }
    println(res)
  }

}
