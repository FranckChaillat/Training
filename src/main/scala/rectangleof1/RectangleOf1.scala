package rectangleof1

object RectangleOf1 {
  type Matrix = Array[Array[Int]]


  def getMaxArea(mat: Matrix): Int = {
    val flagged = collect1s(mat)

    def go(greatestArea: Int, curCol: Int, state: Array[Int]) = {
      if (curCol > flagged.length - 1)
        greatestArea
      else {
        val col = flagged(curCol)
        val gtUpdated = if (col.length > greatestArea) col.length else greatestArea
        val intersection = if (curCol != 0) col.intersect(flagged(curCol - 1)) else Array()
        state.intersect(intersection)
      }
    }
    0
  }

  def collect1s(mat: Matrix): Array[Array[Int]] = {
    for(c <- mat)
      yield c.zipWithIndex.collect {
        case (1, index) => index
      }
  }

  def parseInput(input: String): Array[Matrix] = {
    val rows = input.split("\\n").map(_.stripSuffix("\r"))
    rows.tail.grouped(2)
      .map(tCase => {
        val Array(metadata, values) = tCase
        val Array(row, col) = metadata.split(" ")
        values.split(" ").map(_.toInt).grouped(row.toInt).toArray
      }).toArray
  }

}
