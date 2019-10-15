package bowling

sealed trait Frame {
  def rolls: List[Roll]

  def score: Int
}

case class StrikeFrame(rolls: List[Roll], score: Int) extends Frame

case class SpareFrame(rolls: List[Roll], score: Int) extends Frame

case class OpenFrame(rolls: List[Roll], score: Int) extends Frame
