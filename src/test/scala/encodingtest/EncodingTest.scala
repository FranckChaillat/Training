package encodingtest

import org.scalatest._
import encoding.Encoding

class EncodingTest extends FlatSpec {

  it should "encode given string with 2 rails" in {
    val res = Encoding.encode("ATEST", 2)
    val expected = "A E T\n T S"
    assert(res == expected)
  }


  it should "encode given string with 3 rails" in {
    val res = Encoding.encode("MORECOMPLEX", 3)
    assert(res == "M   C   L\n O E O P E\n  R   M   X")
  }

  it should "encode given string with 5 rails" in {
    val res = Encoding.encode("WEAREDISCOVEREDFLEEATONCE", 5)
    assert(res == "W       C       L       E\n E     S O     F E     C\n  A   I   V   D   E   N\n   R D     E E     A O\n    E       R       T")
  }

  it should "decode given encoded string" in {
    val encodedString = "A E T\n T S"
    val res = Encoding.decode(encodedString, 2)
    assert(res == "ATEST")
  }

  it should "decode given encoded string with 3 rails" in {
    val encodedString = "M   C   L\n O E O P E\n  R   M   X"
    val res = Encoding.decode(encodedString, 3)
    assert(res == "MORECOMPLEX")
  }

  it should "decode given encoded string with 5 rails" in {
    val encodedString = "W       C       L       E\n E     S O     F E     C\n  A   I   V   D   E   N\n   R D     E E     A O\n    E       R       T"
    val res = Encoding.decode(encodedString, 5)
    assert(res == "WEAREDISCOVEREDFLEEATONCE")
  }
}
