package googlecodeJam

object Incidium {

  def getMatrix(diag: Seq[Int]) = {
    
  }


  def getTrace(k: Int, n: Int): Seq[Int] = {
    @scala.annotation.tailrec
    def go(firstTry: Boolean, i: Int): Seq[Int] = {
      if(i <= 0)
        Seq.empty
      else if(firstTry && 1.to(n).sum == k) {
        1.to(n)
      } else if(i * n == k) {
        Seq.fill(n)(i)
      } else {
        go(firstTry = false, i - 1)
      }
    }

    go(true, n)
  }

  def main(args: Array[String]): Unit = {
    val test = getTrace(10, 4)
    println(test)
  }
}
