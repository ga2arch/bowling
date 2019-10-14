package bowling

import scala.annotation.tailrec

sealed trait BowlingGame {
  def score: Int

  def frames: List[Frame]

  def isComplete: Boolean

  def addRoll(roll: Roll): Either[GameError, BowlingGame]

}

sealed trait GameError

case class StandardBowlingGame(frames: List[Frame], score: Int) extends BowlingGame {
  override def addRoll(roll: Roll): Either[GameError, BowlingGame] = {
    validate(roll) match {
      case None =>
        this.frames match {
          case Nil =>
            val frame = newFrame(roll)
            Right(this.copy(frames = List(frame), score = roll.score))

          case initFrames :+ previousFrame =>
            val (updatedFrames, newScore1) = initFrames.lastOption match {
              case Some(frame@StrikeFrame(_, _)) if !frame.isComplete =>
                val updatedFrame = frame addRoll roll
                (initFrames.init :+ updatedFrame, roll.score)

              case _ =>
                (initFrames, 0)
            }
            val (newFrames, newScore2) = previousFrame match {
              case StrikeFrame(_, _) | SpareFrame(_, _) =>
                val updatedFrame = previousFrame addRoll roll
                if (!this.isLastFrame) {
                  (List(updatedFrame, newFrame(roll)), roll.score * 2)
                } else {
                  (List(updatedFrame), roll.score)
                }

              case frame@NormalFrame(_, _) if frame.isComplete =>
                (List(previousFrame, newFrame(roll)), roll.score)

              case frame@NormalFrame(rolls, frameScore) if isSpareRoll(roll) && !frame.isComplete =>
                val spareFrame = SpareFrame(rolls, frameScore) addRoll roll
                (List(spareFrame), roll.score)

              case frame@NormalFrame(_, _) if !frame.isComplete =>
                (List(previousFrame addRoll roll), roll.score)
            }

            Right(this.copy(frames = updatedFrames ++ newFrames, score = this.score + newScore1 + newScore2))
        }

      case Some(error) => Left(error)
    }
  }

  private def validate(roll: Roll): Option[GameError] = {
    if (this.isComplete) return Some(GameIsComplete(this, roll))
    if (this.frames.isEmpty && isSpareRoll(roll)) return Some(InvalidRoll(this, roll))
    if (isSpareRoll(roll) && !isNormalRoll(this.frames.last.rolls.last)) return Some(InvalidRoll(this, roll))

    None
  }

  def isComplete: Boolean = isLastFrame && frames.last.isComplete

  private def isNormalRoll(roll: Roll): Boolean = roll match {
    case NormalRoll(_) => true
    case _ => false
  }

  private def isLastFrame: Boolean = frames.size == 10

  private def isSpareRoll(roll: Roll): Boolean = roll match {
    case SpareRoll(_) => true
    case _ => false
  }

  private def newFrame(roll: Roll): Frame = roll match {
    case NormalRoll(_) => NormalFrame(rolls = List(roll), score = roll.score)
    case SpareRoll(_) => SpareFrame(rolls = List(roll), score = roll.score)
    case StrikeRoll(_) => StrikeFrame(rolls = List(roll), score = roll.score)
  }
}

case class GameIsComplete(game: BowlingGame, roll: Roll) extends GameError

case class InvalidRoll(game: BowlingGame, roll: Roll) extends GameError

object BowlingGame {
  def standard: BowlingGame = StandardBowlingGame(Nil, 0)

  def run[T](game: BowlingGame, lineParser: LineParser[T])(input: T): Either[GameError, BowlingGame] = {
    @tailrec
    def reduce(game: BowlingGame, rolls: Iterator[Roll]): Either[GameError, BowlingGame] = {
      rolls.nextOption() match {
        case Some(roll) => game.addRoll(roll) match {
          case Left(error) => Left(error)
          case Right(newGame) => reduce(newGame, rolls)
        }

        case None => Right(game)
      }
    }

    reduce(game, lineParser.parse(input).iterator)
  }
}

