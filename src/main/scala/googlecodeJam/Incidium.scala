package googlecodeJam

object Incidium {

  type Matrix = Array[Array[Int]]

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

  private def getTrace(k: Int, n: Int) : Seq[Int] = {
    @scala.annotation.tailrec
    def go(acc: Seq[Int], valueAcc: Int, i: Int): Seq[Int] = {
      if(acc.length == n) {
        if(valueAcc != n) Seq.empty else acc
      } else if(acc.isEmpty) {
        val gd = greaterDivisor(n, k, n)
        if(gd == 0)
          Seq()
        else if(gd == n - 1)
          go(Seq.fill(n-2)(n), valueAcc + n * (n-2), i - 1)
        else
          go(Seq.fill(gd)(n), valueAcc + n * (n-1), i - 1)
      } else {
        val placeLeft = n - acc.length
        val gd = greaterDivisor(placeLeft, k - valueAcc, i)
        val newAcc = if(gd == 0) acc else  acc ++ Seq.fill(gd)(i)
        go(newAcc, valueAcc + i * gd, i - 1)
      }
    }

    go(Seq.empty, 0, n)
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
    val test = getTrace(15, 4)
    println(test)
  }
}
