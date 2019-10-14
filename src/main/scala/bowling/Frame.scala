package bowling


sealed trait Frame {
  def rolls: List[Roll]

  def score: Int

  def isComplete: Boolean

  def addRoll(roll: Roll): Frame
}

case class StrikeFrame(rolls: List[Roll], score: Int) extends Frame {
  override def addRoll(roll: Roll): Frame = {
    val rolls = this.rolls :+ roll
    val score = this.score + roll.score

    StrikeFrame(rolls, score)
  }

  override def isComplete: Boolean = rolls.size == 3
}

case class SpareFrame(rolls: List[Roll], score: Int) extends Frame {
  override def addRoll(roll: Roll): Frame = {
    val rolls = this.rolls :+ roll
    val score = this.score + roll.score

    SpareFrame(rolls, score)
  }

  override def isComplete: Boolean = rolls.size == 3
}

case class NormalFrame(rolls: List[Roll], score: Int) extends Frame {
  override def addRoll(roll: Roll): Frame = {
    val rolls = this.rolls :+ roll
    val score = this.score + roll.score

    NormalFrame(rolls, score)
  }

  override def isComplete: Boolean = rolls.size == 2
}