package subSeqNegProduct

object SubSeqNegProduct extends App {
  def getNegativeProducts(list: Seq[Int]): Int = {
    val negativeCount = list.count(_ < 0)
    val positiveCount = list.size - negativeCount
    val combination = if(positiveCount == 1) positiveCount else (positiveCount * (positiveCount - 1)) / 2
    (combination * negativeCount) + negativeCount
  }


  println(getNegativeProducts(Seq(-1,2,-8,0,4,20,-6)))
  println(getNegativeProducts(Seq(55, 2, -1, 4, 8)))
  println(getNegativeProducts(Seq(1, -5, -6)))
  println(getNegativeProducts(Seq(2, 3, 1)))


}
