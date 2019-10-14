package bowling

import org.scalatest._

class BowlingGameTest extends FlatSpec with Matchers {
  val lineToScore = Map(
    "X X X X X X X X X XXX" -> 300,
    "X -/ X 5- 8/ 9- X 81 1- 4/X" -> 137,
    "62 71 X 9- 8/ X X 35 72 5/8" -> 140,
    "X 7/ 72 9/ X X X 23 6/ 7/3" -> 168,
    "X X X X 9/ X X 9/ 9/ XXX" -> 247,
    "8/ 54 9- X X 5/ 53 63 9/ 9/X" -> 149,
    "X 7/ 9- X -8 8/ -6 X X X81" -> 167,
    "X 9/ 5/ 72 X X X 9- 8/ 9/X" -> 187,
    "X -/ X X X X X X X XXX" -> 280,
    "X 1/ X X X X X X X XXX" -> 280,
    "X 2/ X X X X X X X XXX" -> 280,
    "X 3/ X X X X X X X XXX" -> 280,
    "X 4/ X X X X X X X XXX" -> 280,
    "X 5/ X X X X X X X XXX" -> 280,
    "X 6/ X X X X X X X XXX" -> 280,
    "X 7/ X X X X X X X XXX" -> 280,
    "X 8/ X X X X X X X XXX" -> 280,
    "X 9/ X X X X X X X XXX" -> 280,
    "-/ X X X X X X X X XX-" -> 280,
    "1/ X X X X X X X X XX-" -> 280,
    "2/ X X X X X X X X XX-" -> 280,
    "3/ X X X X X X X X XX-" -> 280,
    "4/ X X X X X X X X XX-" -> 280,
    "5/ X X X X X X X X XX-" -> 280,
    "6/ X X X X X X X X XX-" -> 280,
    "7/ X X X X X X X X XX-" -> 280,
    "8/ X X X X X X X X XX-" -> 280,
    "9/ X X X X X X X X XX-" -> 280,
    "X X X X X X X X X X-/" -> 280,
    "X X X X X X X X X X18" -> 280,
    "X X X X X X X X X X26" -> 280,
    "X X X X X X X X X X34" -> 280,
    "X X X X X X X X X X42" -> 280,
    "X X X X X X X X X X5-" -> 280,
    "-/ X X X X X X X X XX1" -> 281,
    "1/ X X X X X X X X XX1" -> 281,
    "2/ X X X X X X X X XX1" -> 281,
    "3/ X X X X X X X X XX1" -> 281,
    "4/ X X X X X X X X XX1" -> 281,
    "5/ X X X X X X X X XX1" -> 281,
    "6/ X X X X X X X X XX1" -> 281,
    "7/ X X X X X X X X XX1" -> 281,
    "8/ X X X X X X X X XX1" -> 281,
    "9/ X X X X X X X X XX1" -> 281,
    "X X X X X X X X X X1/" -> 281,
    "X X X X X X X X X X27" -> 281,
    "X X X X X X X X X X35" -> 281,
    "X X X X X X X X X X43" -> 281,
    "X X X X X X X X X X51" -> 281
  )

  "A BowlingGame" should "calculate score for lines" in {
      lineToScore.foreach(lineScore => {
        val result = BowlingGame.run(BowlingGame.standard, LineParser.string)(lineScore._1)
        result.isRight shouldBe true
        result.right.get.score shouldBe lineScore._2
      })
  }
}
