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

  def getMasks(trace: Seq[Int]): Mask = {
    trace.indices.map (i => {
      1.until(trace.length +1)
       .filter(j => j != trace(i)).toSet
    }).toArray
  }

  def getDispatch(rowMask: Mask, colMask: Mask, currentRow: Int): Seq[(Int, Set[Int])] = {
    rowMask(currentRow).map(value => {
      val positions = colMask.zipWithIndex.collect {
        case (valueSet, index) if index != currentRow && valueSet.contains(value) => index
      }.toSet

      (value, positions)
    }).toSeq
  }

  private def getFilledRow(traceValue: Int, traceIndex: Int, dispatch: Seq[(Int, Set[Int])]): Array[Int] = {
    @scala.annotation.tailrec
    def go(acc: Array[Int], dispatch: Seq[(Int, Set[Int])], i: Int): Array[Int] = {
      dispatch.headOption match {
        case Some(_) if i == traceIndex =>
          go(acc, dispatch, i + 1)
        case Some((value, positions)) =>
          val p = positions.head
          val newDispatch = dispatch.tail.map(x => x.copy(_2 = x._2 - p)).sortBy(_._2.size)
          go(acc.updated(p, value), newDispatch, i + 1)
        case None => acc
      }
    }

    val start: Array[Int] = (for(i <- 0 to dispatch.length) yield if (i == traceIndex) traceValue else 0).toArray
    go(start, dispatch, 0)
  }

  def fillMatrix(trace: Seq[Int]): Matrix = {
    @scala.annotation.tailrec
    def buildResult(acc: Matrix, rowMask: Mask, colMask: Mask): Matrix = {
      if(acc.length == trace.length)
        acc
      else {
        val currentRow = acc.length
        val possibleDispatch = getDispatch(rowMask, colMask, currentRow).sortBy(x => x._2.size)
        val filledRow = getFilledRow(traceValue = trace(currentRow), traceIndex = currentRow, possibleDispatch)
        //update colMask
        val updatedColMask : Mask = filledRow.zip(colMask).map { case (value, columnValues) => columnValues.-(value) }
        buildResult(acc.:+(filledRow), rowMask, updatedColMask)
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
      if(trace.isEmpty)
        None
      else
        Some(fillMatrix(trace))
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

