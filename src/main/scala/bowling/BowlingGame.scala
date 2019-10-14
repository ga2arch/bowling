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
      case None => Right(addRollToGame(roll))
      case Some(error) => Left(error)
    }
  }

  private def addRollToGame(roll: Roll): BowlingGame =
    frames match {
      case Nil =>
        val frame = newFrame(roll)
        this.copy(frames = List(frame), score = roll.score)

      case initFrames :+ previousFrame =>
        val (updatedFrames, newScore1) = initFrames.lastOption match {
          case Some(frame@StrikeFrame(_, _)) if !isFrameComplete(frame) =>
            val updatedFrame = addRollToFrame(frame, roll)
            (initFrames.init :+ updatedFrame, roll.score)

          case _ =>
            (initFrames, 0)
        }
        val (newFrames, newScore2) = previousFrame match {
          case StrikeFrame(_, _) | SpareFrame(_, _) =>
            val updatedFrame = addRollToFrame(previousFrame, roll)
            if (!this.isLastFrame) {
              (List(updatedFrame, newFrame(roll)), roll.score * 2)
            } else {
              (List(updatedFrame), roll.score)
            }

          case frame@OpenFrame(_, _) if isFrameComplete(frame) =>
            (List(previousFrame, newFrame(roll)), roll.score)

          case frame@OpenFrame(_, _) if !isFrameComplete(frame) =>
            (List(addRollToFrame(previousFrame, roll)), roll.score)
        }

        this.copy(frames = updatedFrames ++ newFrames, score = this.score + newScore1 + newScore2)
    }


  private def addRollToFrame(frame: Frame, roll: Roll) = frame match {
    case OpenFrame(_, _) if isSpareRoll(roll) => SpareFrame(frame.rolls :+ roll, score = frame.score + roll.score)
    case OpenFrame(_, _) => OpenFrame(frame.rolls :+ roll, score = frame.score + roll.score)
    case StrikeFrame(_, _) => StrikeFrame(frame.rolls :+ roll, score = frame.score + roll.score)
    case SpareFrame(_, _) => SpareFrame(frame.rolls :+ roll, score = frame.score + roll.score)
  }

  private def isFrameComplete(frame: Frame) = frame match {
    case OpenFrame(rs, _) => rs.size == 2
    case StrikeFrame(rs, _) => rs.size == 3
    case SpareFrame(rs, _) => rs.size == 3
  }

  private def validate(roll: Roll): Option[GameError] = {
    if (this.isComplete) return Some(GameIsComplete(this, roll))
    if (this.frames.isEmpty && isSpareRoll(roll)) return Some(InvalidRoll(this, roll))
    if (isSpareRoll(roll) && !isOpenRoll(this.frames.last.rolls.last)) return Some(InvalidRoll(this, roll))

    None
  }

  def isComplete: Boolean = isLastFrame && isFrameComplete(frames.last)

  private def isOpenRoll(roll: Roll): Boolean = roll match {
    case OpenRoll(_) => true
    case _ => false
  }

  private def isLastFrame: Boolean = frames.size == 10

  private def isSpareRoll(roll: Roll): Boolean = roll match {
    case SpareRoll(_) => true
    case _ => false
  }

  private def newFrame(roll: Roll): Frame = roll match {
    case OpenRoll(_) => OpenFrame(rolls = List(roll), score = roll.score)
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

