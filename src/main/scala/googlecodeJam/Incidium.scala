package googlecodeJam

object Incidium {

  type Matrix = Array[Array[Int]]
  type Mask = Array[Set[Int]]
  @scala.annotation.tailrec
  private def greaterDivisor(placeLeft: Int, dividend: Int, divisor: Int) :Int = {
    if(divisor <= 0)
      0
    else {
      val gd = Math.floor(dividend / divisor).toInt
      if (gd > placeLeft) {
        greaterDivisor(placeLeft, dividend, divisor - 1)
      } else {
        gd
      }
    }
  }

  def getTrace(k: Int, n: Int) : Seq[Int] = {
    @scala.annotation.tailrec
    def go(acc: Seq[Int], valueAcc: Int, i: Int): Seq[Int] = {
      val placeLeft = n - acc.length
      if(acc.length == n && valueAcc != k)
        Seq.empty
      else if(acc.length == n)
       acc
      else if(acc.isEmpty) {
        val gd = greaterDivisor(n, k, n)
        if(gd == 0)
          Seq.empty
        else if(gd == n - 1)
          go(Seq.fill(n-2)(n), valueAcc + n * (n-2), i - 1)
        else if((k - valueAcc) - (gd * i)  < (placeLeft - gd))
          go(acc, valueAcc, i - 1)
        else
          go(Seq.fill(gd)(i), valueAcc + i * gd, i)
      } else {
        val gd = greaterDivisor(placeLeft, k - valueAcc, i)
        val newAcc = if((k - valueAcc) - (gd * i)  < placeLeft - gd) acc else acc ++ Seq.fill(gd)(i)
        go(newAcc, newAcc.sum, i - 1)
      }
    }

    go(Seq.empty, 0, n)
  }

  private def getMasks(trace: Seq[Int]): Mask = {
    trace.foldLeft(Array.empty[Set[Int]])((acc, e) => {
      val set = (for(i <- trace.indices if i != e) yield i).toSet
      acc.:+(set)
    })
  }

  def fillMatrix(trace: Seq[Int]): Matrix = {
    def buildResult(acc: Matrix, rowMask: Mask, colMask: Mask): Matrix = {
      if(acc.length == trace.length)
        acc
      else {
        val currentRow = trace.length - acc.length
        currentRow
      }

    }

    val mask = getMasks(trace)
    buildResult(Array.empty, mask, mask)
  }

  def tryGetSolution(n: Int, k: Int) : Option[Matrix] = {
    if(n > k || Math.pow(n, 2) < k) {
      None
    } else {
      val trace = getTrace(k, n)
      None
    }
  }

  def getSamples(input: String): Array[(Int, Int)] = {
    val rows = input.split("\\n")
    rows.tail.map(row => {
      val splitted = row.split(" ")
      (splitted(0).toInt, splitted(1).toInt)
    })
  }

  def process(input: String) = {
    val samples = getSamples(input)

  }


  def main(args: Array[String]): Unit = {
    val test = getTrace(10, 4)
    println(test)
  }
}
