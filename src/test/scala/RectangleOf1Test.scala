import org.scalatest.FlatSpec
import rectangleof1.RectangleOf1

class RectangleOf1Test extends FlatSpec {

  it should "build 2 matrices from input" in {
    val input = """|2
                   |2 3
                   |1 1 1 0 0 0
                   |2 2
                   |1 1 1 1
                   |""".stripMargin

    val result = RectangleOf1.parseInput(input)
    assert(result.length == 2)
    assert(result.head.length == 3 && result.head.forall(_.length == 2))
    assert(result(1).length == 2 && result(1).forall(_.length == 2))
  }


  it should "flag the indexes where the 1 are in the following matrice" in {
    val input =
      """|1
         |2 2
         |1 1 1 1
         |""".stripMargin

    val parsed = RectangleOf1.parseInput(input)
    val flaged1 = RectangleOf1.collect1s(parsed.head)
    assert(flaged1.length == 2)
    assert(flaged1.head sameElements Array(0, 1))
    assert(flaged1(1) sameElements Array(0, 1))
  }

  it should "flag again the indexes where the 1 are in the following matrice" in {
    val input =
      """|1
         |3 4
         |0 0 0 1 1 0 0 1 0 1 1 0
         |""".stripMargin
    val parsed = RectangleOf1.parseInput(input)
    val flaged1 = RectangleOf1.collect1s(parsed.head)
    assert(flaged1.length == 4)
    assert(flaged1.head.isEmpty)
    assert(flaged1(1) sameElements Array(0, 1))
    assert(flaged1(2) sameElements Array(1))
    assert(flaged1(3) sameElements Array(0, 1))
  }

}
