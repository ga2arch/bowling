package bowling

import org.scalatest._

class StandardBowlingGameTest extends FlatSpec with Matchers {
  private val lineToScore = List(
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
        //when
        val game = runGame(lineScore._1)

        //then
        game.isComplete shouldBe true
        game.frames.size shouldBe 10
        game.score shouldBe lineScore._2
      })
  }

  it should "return GameIsComplete when adding a roll to a complete game" in {
    //given
    val game = runGame(lineToScore.head._1)
    val roll = StrikeRoll(10)

    //when
    val result = game.addRoll(StrikeRoll(10))

    //then
    result.isLeft shouldBe true
    result shouldBe Left(GameIsComplete(game, roll))
  }

  it should "return InvalidRoll when the addition of a roll is invalid related to the game state" in {
    //given
    val game = BowlingGame.standard
    val roll = SpareRoll(10)

    //when
    val result = game.addRoll(roll)

    //then
    result.isLeft shouldBe true
    result shouldBe Left(InvalidRoll(game, roll))
  }

  "The sum of the score of each frame of a BowlingGame" should "be the same score of the game" in {
    lineToScore.map(_._1).foreach(line => {
      //when
      val game = runGame(line)

      //then
      game.isComplete shouldBe true
      game.score shouldBe game.frames.map(_.score).sum
    })
  }

  private def runGame(line: String) = BowlingGame.run(BowlingGame.standard, LineParser.string)(line).right.get

}
