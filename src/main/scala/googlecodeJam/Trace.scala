package googlecodeJam

import utils.Utils

import scala.util.Random

object Trace {

  type Matrix = Array[Array[Int]]

  def getTrace(input: String) : Array[(Int, Int, Int)] = {
    val params = input.split("\\n").map(_.stripSuffix("\r"))
    val matrices = getMatrices(params(0).toInt, params.tail, Array())
    matrices.par.map { m =>
      val (r, c) = getRepetition(m)
      (getTrace(m), r, c)
    }.seq.toArray
  }

  private def getTrace(mat: Matrix): Int = {
      for (i <- mat.indices)
        yield mat(i)(i)
    }.sum

  private def hasRepetition(input: Array[Int]) : Boolean = {
    @scala.annotation.tailrec
    def go(values : Array[Int], state: Set[Int]): Boolean = {
       if(values.isEmpty)
         false
       else if(state.contains(values(0)))
         true
       else {
         go(values.tail, state.+(values(0)))
       }
    }

    go(input, Set())
  }

  private def getRepetition(mat: Matrix): (Int, Int) = {
    val r = mat.map(hasRepetition).count(p => p)
    val rotated: Matrix = { for(i <- mat.indices)
        yield { for (j <- mat(i).indices)
          yield mat(j)(i)
        }.toArray
      }.toArray

    val c = rotated.map(hasRepetition).count(p => p)
    (r, c)
  }

  @scala.annotation.tailrec
  private def getMatrices(t: Int, in : Array[String], acc: Array[Matrix]): Array[Matrix] = {
    if(t == 0) acc
    else {
      val len = in.head.toInt
      val mat = in.tail.slice(0, len).map(row => row.split(" ").map(_.toInt))
      val tail = in.slice(len + 1, in.length)
      getMatrices(t - 1, tail, acc.:+(mat))
    }
  }


  def main(args: Array[String]): Unit = {
    val input =
      """|3
         |4
         |1 2 3 4
         |2 1 4 3
         |3 4 1 2
         |4 3 2 1
         |4
         |2 2 2 2
         |2 3 2 3
         |2 2 2 3
         |2 2 2 2
         |3
         |2 1 3
         |1 3 2
         |1 2 3""".stripMargin


    val rnd = new Random()
   val t =  (for(_ <-0 until 5000)
      yield (for(_ <- 0 until 5000)
        yield rnd.nextInt(5000)).mkString(" "))

   val i =
     s"""|1
         |5000
         |${t.mkString("\n")}
         |""".stripMargin

    println("start processing...")
    val res = Utils.time {
       getTrace(i)
    }

    for ((k, r, c) <- res) {
      println(s"$k, $r, $c")
    }
  }


}
