package encoding

import scala.annotation.tailrec


object Test extends App {
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  val longSample = (0 to 1000000).foldLeft(new StringBuilder())((acc, _) => acc.append(alphabet)).toString()
  utils.Utils.time {
    print("start now ...")
    Encoding.encode(longSample, 5)
  }
}


object Encoding{

  def encode(str: String, railCount: Int): String = {
    val resolveDirection = getDirection(railCount)_
    @tailrec
    def format(acc: Array[Array[Char]], pos: Int, rail: Int, direction : Int => Int): Array[Array[Char]] = {
      if(pos >= str.length)
        acc
      else {
        acc(rail).update(pos, str(pos))
        val newRailPosition = direction(rail)
        val newDirection = resolveDirection(newRailPosition, direction)
        format(acc, pos + 1, newRailPosition, newDirection)
      }
    }

    val acc = Array.fill(railCount, str.length)(' ')
    val railEncoded = format(acc, 0, 0, x => x+1)
    toString(railEncoded)
  }

  def decode(encoded: String, railCount: Int) : String = {
    val resolveDirection = getDirection(railCount)_

    val encodedArray = encoded.split("\n").map(_.toCharArray)

    @tailrec
    def go(acc: StringBuilder, pos: Int, rail: Int, direction : Int => Int): String = {
      val line = encodedArray(rail)
      if(pos >= line.length)
        acc.toString()
      else {
        val newAcc = acc.append(line(pos))
        val newRailPosition = direction(rail)
        val newDirection = resolveDirection(newRailPosition, direction)
        go(newAcc, pos + 1, newRailPosition, newDirection)
      }
    }

    go(new StringBuilder, 0, 0, x => x + 1)
  }

  private def getDirection(railCount: Int)(currentPos: Int, dirFunc: Int => Int) = {
    val nextPos = dirFunc(currentPos)
    if(nextPos > railCount - 1)
      (p: Int) => p - 1
    else if(nextPos < 0)
      (p: Int) => p + 1
    else
     dirFunc
  }


  private def toString(railEncoded: Array[Array[Char]]): String = {
    val regex = "[ \\t]+$".r
    val rails = railEncoded.par.map(rail => regex.replaceFirstIn(rail.mkString, ""))
    rails.mkString("\n")
  }

}