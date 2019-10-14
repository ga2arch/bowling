package bowling

import org.scalatest._

class StringLineParserTest extends FlatSpec with Matchers {

  "A StringLineParser" should "transform a X in StrikeRoll(10)" in {
    //given
    val line = "X"

    //when
    val result = LineParser.string.parse(line).iterator.toList

    //then
    result shouldBe List(StrikeRoll(10))
  }

  it should "transform a - in OpenRoll(0)" in {
    //given
    val line = "-"

    //when
    val result = LineParser.string.parse(line).iterator.toList

    //then
    result shouldBe List(OpenRoll(0))
  }

  it should "transform a [1-9] in OpenRoll([1-9])" in {
    //given
    val line = "9"

    //when
    val result = LineParser.string.parse(line).iterator.toList

    //then
    result shouldBe List(OpenRoll(9))
  }

  it should "transform a [1-9]/ in SpareRoll(10 - [1-9])" in {
    //given
    val line = "9/"

    //when
    val result = LineParser.string.parse(line).iterator.toList

    //then
    result shouldBe List(OpenRoll(9), SpareRoll(1))
  }
}
