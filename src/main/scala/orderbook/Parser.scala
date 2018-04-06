package orderbook

import scala.util.Try

case class Parser[T](op: String => Try[T])

object Parser {
  implicit val parseInt: Parser[Int] = Parser { s => Try { s.toInt } }
  implicit val parseDouble: Parser[Double] = Parser { s => Try { s.toDouble } }

  def parse[T: Parser](s: String): Try[T] = implicitly[Parser[T]].op(s)
}