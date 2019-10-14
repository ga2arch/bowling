import bowling.{BowlingGame, LineParser}

object Main {

  def main(args: Array[String]): Unit = {
    Iterator.continually(io.StdIn.readLine)
      .takeWhile(line => !line.isEmpty)
      .foreach(line => {
        runStandardGame(line) match {
          case Right(game) => println(s"$line\tscore: ${game.score}")
          case Left(error) => println(s"$line\terror: $error")
        }
      })
  }

  private def runStandardGame(line: String) = BowlingGame.run(BowlingGame.standard, LineParser.string)(line)
}
