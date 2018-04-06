package orderbook

import scala.io.Source
import scala.util.{Failure, Success, Try}

object Program {
  final val HelpUsageMessage: String = "Usage: sbt> run orders_filename tick_size book_depth"
  final val HelpTickSizeMessage: String = "Argument tick_size must be a decimal number, e.g. 10.0"
  final val HelpBookDepthMessage: String = "Argument book_depth must be an integer number, e.g. 2"

  def main(args: Array[String]): Unit = {
    val book: Try[OrderBook] = for {
      (filename, tickSize, bookDepth) <- parseArgs(args)
      result <- TryWith(Source.fromFile(filename)) { source =>
        processOrders(source, OrderBook(tickSize, bookDepth))
      }
    } yield result

    book match {
      case Success(b) => println(OrderBook.show(b))
      case Failure(e) => Console.err.println(s"${e.getMessage()}")
    }
  }

  def parseArgs(args: Array[String]): Try[(String, Double, Int)] =
    if (args.length != 3) Failure(new IllegalArgumentException(HelpUsageMessage))
    else {
      val filename = args(0)
      for {
        tickSize <- Parser.parse[Double](args(1)) recoverWith { 
          case _ => Failure(new NumberFormatException(HelpTickSizeMessage))
        }
        bookDepth <- Parser.parse[Int](args(2)) recoverWith { 
          case _ => Failure(new NumberFormatException(HelpBookDepthMessage))
        }
      } yield (filename, tickSize, bookDepth)
    }

  def processOrders(source: Source, genesis: OrderBook): OrderBook =
    source
      .getLines()
      .toStream
      .map { Order.parse(_) }
      .collect { case Success(o) => o } // Discard order parsing errors
      .foldLeft(genesis) { (b, o) => OrderBook.processOrder(o, b) }
}
