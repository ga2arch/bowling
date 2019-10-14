package bowling

import scala.collection.IterableOnce
import scala.collection.immutable.LazyList.cons

sealed trait LineParseError

case object InvalidToken extends LineParseError

sealed trait LineParser[T] {
  def parse(line: T): IterableOnce[Roll]
}

class StringLineParser extends LineParser[String] {
  override def parse(line: String): IterableOnce[Roll] = {
    cons(' ', line.to(LazyList).filterNot(c => c == ' '))
      .sliding(2)
      .map(w => tokenToRoll(w.head, w.last))
  }

  private def tokenToRoll(prev: Char, token: Char) = (prev, token) match {
    case (_, 'X') =>
      StrikeRoll(10)

    case ('-', '/') =>
      SpareRoll(10)

    case (previous, '/') =>
      SpareRoll(10 - (previous - '0'))

    case (_, '-') =>
      NormalRoll(0)

    case (_, c) =>
      NormalRoll(c - '0')
  }
}

object LineParser {

  def string: StringLineParser = {
    new StringLineParser()
  }
}
