package orderbook

case class PriceLevel(price: Double, quantity: Int)

case class OrderBook(tickSize: Double, depth: Int, bids: Map[Int, PriceLevel], asks: Map[Int, PriceLevel])

object OrderBook {
  def apply(tickSize: Double, depth: Int): OrderBook = OrderBook(tickSize, depth, Map.empty, Map.empty)

  def processOrder(order: Order, book: OrderBook): OrderBook = {
    if (order.index <= 0 || order.index > book.depth) book
    else order match {
      case NewOrder(Bid, index, ticks, quantity) =>
        val bidsToUpdate: Map[Int, PriceLevel] =
          if (book.bids.contains(index)) shiftUp(index, book.depth, book.bids)
          else book.bids
        book.copy(bids = bidsToUpdate + (index -> PriceLevel(ticks * book.tickSize, quantity)))
      case NewOrder(Ask, index, ticks, quantity) =>
        val asksToUpdate: Map[Int, PriceLevel] =
          if (book.asks.contains(index)) shiftUp(index, book.depth, book.asks)
          else book.asks
        book.copy(asks = asksToUpdate + (index -> PriceLevel(ticks * book.tickSize, quantity)))

      case UpdateOrder(Bid, index, ticks, quantity) if book.bids.contains(index) =>
        book.copy(
          bids = book.bids +
            (index -> PriceLevel(ticks * book.tickSize, quantity)))
      case UpdateOrder(Ask, index, ticks, quantity) if book.asks.contains(index) =>
        book.copy(
          asks = book.asks +
            (index -> PriceLevel(ticks * book.tickSize, quantity)))

      case DeleteOrder(Bid, index) if book.bids.contains(index) =>
        book.copy(bids = shiftDown(index, book.bids - index))
      case DeleteOrder(Ask, index) if book.bids.contains(index) =>
        book.copy(asks = shiftDown(index, book.asks - index))

      case _ => book
    }
  }

  def show(book: OrderBook): String = 
    (for {
      index <- 1 to book.depth
      bid = book.bids.getOrElse(index, PriceLevel(0.0, 0))
      ask = book.asks.getOrElse(index, PriceLevel(0.0, 0))
    } yield s"${bid.price},${bid.quantity},${ask.price},${ask.quantity}").mkString("\n")

  private def shiftUp(index: Int, maxIndex: Int, m: Map[Int, PriceLevel]): Map[Int, PriceLevel] =
    mapKeysAndValues(m)(i => if (i >= index) i + 1 else i)(identity) filterKeys { _ <= maxIndex }

  private def shiftDown(index: Int, m: Map[Int, PriceLevel]): Map[Int, PriceLevel] =
    mapKeysAndValues(m)(i => if (i > index) i - 1 else i)(identity)

  private def mapKeysAndValues[A, B, C, D](m: Map[A, B])(f: A => C)(g: B => D): Map[C, D] =
    m map { case (k, v) => (f(k), g(v)) }
}