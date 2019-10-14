package bowling


sealed trait Roll {
  def score: Int
}

case class StrikeRoll(score: Int) extends Roll

case class SpareRoll(score: Int) extends Roll

case class NormalRoll(score: Int) extends Roll
