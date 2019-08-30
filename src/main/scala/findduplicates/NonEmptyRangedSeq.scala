package findduplicates

import scala.annotation.tailrec

trait RangedSeq
case class NonEmptyRangedSeq(head : (Int, Int), tail: RangedSeq) extends RangedSeq
case object EmptyRangedSeq extends RangedSeq

object NonEmptyRangedSeq {

    def apply(seq : Seq[Int]): RangedSeq = {
      @tailrec
      def go(seq: Seq[Int], acc: RangedSeq): RangedSeq = {
        seq match {
          case e +: seqTail =>
            acc match {
              case current@NonEmptyRangedSeq((lower, upper), tail) =>
                val newValue = if (e == upper + 1) NonEmptyRangedSeq(lower -> e, tail) else NonEmptyRangedSeq( e -> e, current)
                go(seqTail, newValue)
              case EmptyRangedSeq =>
                NonEmptyRangedSeq((e, e), EmptyRangedSeq)
            }
          case Seq() =>
            EmptyRangedSeq
        }

      }
      go(seq, EmptyRangedSeq)
    }


//      seq.headOption.map { h =>
//        seq.tail.foldLeft(Seq((h, h)))((acc, e) => {
//          acc match {
//            case (lower, upper) +: t =>
//              if(e == upper + 1)
//                t.+:((lower, e))
//              else
//                acc.+:(e, e)
//          }
//        }).reverse
//      }
    }

  def insert(rangedSeq : NonEmptyRangedSeq)(n : Int) : NonEmptyRangedSeq = {
    def go(seq : Seq[(Int, Int)], acc : Seq[(Int, Int)]) : Seq[(Int, Int)] = {
      seq match {
        case (lower, upper) +: tail =>
          if(n >= lower && n <= upper)
            acc.reverse ++ seq
          else if(n == lower-1)
            go(tail, acc.+:((n, upper)))
        case Seq() =>
          acc
      }
    }
  }
}