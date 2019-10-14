package bowling

import scala.annotation.tailrec

sealed trait Game {
  def score: Int

  def frames: List[Frame]

  def isComplete: Boolean

  def addRoll(roll: Roll): Either[GameError, Game]

}

sealed trait GameError

case class StandardGame(frames: List[Frame], score: Int) extends Game {
  override def addRoll(roll: Roll): Either[GameError, Game] = {
    validate(roll) match {
      case None => Right(processRoll(roll))
      case Some(error) => Left(error)
    }
  }

  private def processRoll(roll: Roll): Game =
    frames match {
      case Nil =>
        val frame = newFrame(roll)
        this.copy(frames = List(frame), score = roll.score)

      case initFrames :+ previousFrame =>
        val (updatedFrames, newScore1) = initFrames.lastOption match {
          case Some(frame@StrikeFrame(_, _)) if !isFrameComplete(frame) =>
            val updatedFrame = addRoll(frame, roll)
            (initFrames.init :+ updatedFrame, roll.score)

          case _ =>
            (initFrames, 0)
        }
        val (newFrames, newScore2) = previousFrame match {
          case StrikeFrame(_, _) | SpareFrame(_, _) =>
            val updatedFrame = addRoll(previousFrame, roll)
            if (!this.isLastFrame) {
              (List(updatedFrame, newFrame(roll)), roll.score * 2)
            } else {
              (List(updatedFrame), roll.score)
            }

          case frame@OpenFrame(_, _) if isFrameComplete(frame) =>
            (List(previousFrame, newFrame(roll)), roll.score)

          case frame@OpenFrame(_, _) if !isFrameComplete(frame) =>
            (List(addRoll(previousFrame, roll)), roll.score)
        }

        this.copy(frames = updatedFrames ++ newFrames, score = this.score + newScore1 + newScore2)
    }


  private def addRoll(frame: Frame, roll: Roll) = {
    val newRolls = frame.rolls :+ roll
    val newScore = frame.score + roll.score

    frame match {
      case OpenFrame(_, _) if isSpareRoll(roll) => SpareFrame(newRolls, newScore)
      case OpenFrame(_, _) => OpenFrame(newRolls, newScore)
      case StrikeFrame(_, _) => StrikeFrame(newRolls, newScore)
      case SpareFrame(_, _) => SpareFrame(newRolls, newScore)
    }
  }

  private def isFrameComplete(frame: Frame) = frame match {
    case OpenFrame(rs, _) => rs.size == 2
    case StrikeFrame(rs, _) => rs.size == 3
    case SpareFrame(rs, _) => rs.size == 3
  }

  private def validate(roll: Roll): Option[GameError] = {
    if (this.isComplete) {
      Some(GameIsComplete(this, roll))

    } else if (this.frames.isEmpty && isSpareRoll(roll)) {
      Some(InvalidRoll(this, roll))

    } else if (isSpareRoll(roll) && !isOpenRoll(this.frames.last.rolls.last)) {
      Some(InvalidRoll(this, roll))

    } else {
      None
    }
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

case class GameIsComplete(game: Game, roll: Roll) extends GameError

case class InvalidRoll(game: Game, roll: Roll) extends GameError

object Game {
  def standard: Game = StandardGame(Nil, 0)

  def run[T](game: Game, lineParser: LineParser[T])(input: T): Either[GameError, Game] = {
    @tailrec
    def reduce(game: Game, rolls: Iterator[Roll]): Either[GameError, Game] = {
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

