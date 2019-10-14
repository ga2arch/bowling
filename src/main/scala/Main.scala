import bowling.{BowlingGame, LineParser}

object Main {

  def main(args: Array[String]): Unit = {
    println("Bowling line score calculator")
    Iterator.continually(prompt())
      .takeWhile(line => !line.isEmpty)
      .foreach(line => {
        runStandardGame(line) match {
          case Right(game) => println(game.score)
          case Left(error) => println(s"$line\terror: $error")
        }
      })
  }

  private def prompt() = {
    print("> ")
    io.StdIn.readLine()
  }

  private def runStandardGame(line: String) = BowlingGame.run(BowlingGame.standard, LineParser.string)(line)
}
