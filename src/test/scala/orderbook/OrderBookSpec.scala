package orderbook

import org.scalatest.{Matchers, WordSpec}
import OrderBook._

class OrderBookSpec extends WordSpec with Matchers {
  val emptyOrderBook: OrderBook = OrderBook(10.0, 2)

  val newBidOrder: Order = NewOrder(Bid, 1, 1, 10)
  val newAskOrder: Order = NewOrder(Ask, 1, 1, 10)
  val updateBidOrder: Order = UpdateOrder(Bid, 1, 2, 20)
  val updateAskOrder: Order = UpdateOrder(Ask, 1, 2, 20)
  val deleteBidOrder: Order = DeleteOrder(Bid, 1)
  val deleteAskOrder: Order = DeleteOrder(Ask, 1)

  "OrderBook" when {
    "empty" should {
      "have no bids" in {
        emptyOrderBook.bids.size shouldBe 0
      }

      "have no asks" in {
        emptyOrderBook.bids.size shouldBe 0
      }

      "output price levels not provided as values of 0" in {
        show(emptyOrderBook) shouldBe "0.0,0,0.0,0\n0.0,0,0.0,0"
      }
    }

    "processing orders" should {
      "add new orders" in {
        val b = processOrder(newBidOrder, emptyOrderBook)
        b.bids(1) shouldBe PriceLevel(10.0, 10)
      }

      "add delete orders" in {
        val b = List(
          NewOrder(Bid, 1, 1, 10),
          NewOrder(Ask, 1, 1, 10)
        ).foldLeft(OrderBook(10.0, 2))((b, o) => processOrder(o, b))
        processOrder(deleteAskOrder, processOrder(deleteBidOrder, b))
          .bids shouldBe Map.empty
      }

      "add update orders" in {
        val b = processOrder(newBidOrder, emptyOrderBook)
        processOrder(updateBidOrder, b).bids(1) shouldBe PriceLevel(20.0, 20)
      }

      "handle price level indexes not in order" in {
        val b = List(
          NewOrder(Bid, 1, 1, 10),
          NewOrder(Bid, 3, 3, 30),
          NewOrder(Bid, 2, 2, 20)
        ).foldLeft(OrderBook(10.0, 3))((b, o) => processOrder(o, b))
        b.bids.size shouldBe 3
        List(
          (1 -> PriceLevel(10.0, 10)),
          (2 -> PriceLevel(20.0, 20)),
          (3 -> PriceLevel(30.0, 30))
        ).forall { case (i, p) => b.bids(i) == p } shouldBe true
      }

      "shift up existing price levels with equal or greater index" in {
        val b = List(
          NewOrder(Ask, 1, 1, 10),
          NewOrder(Ask, 2, 2, 20),
          NewOrder(Ask, 1, 10, 100)
        ).foldLeft(OrderBook(10.0, 2))((b, o) => processOrder(o, b))
        b.asks.size shouldBe 2
        List(
          (1 -> PriceLevel(100.0, 100)),
          (2 -> PriceLevel(10.0, 10)),
        ).forall { case (i, p) => b.asks(i) == p } shouldBe true
      }

      "shift down existing price levels with higher index" in {
        val b = List(
          NewOrder(Bid, 1, 1, 10),
          NewOrder(Bid, 2, 2, 20),
          NewOrder(Bid, 3, 3, 30),
          DeleteOrder(Bid, 2)
        ).foldLeft(OrderBook(10.0, 3))((b, o) => processOrder(o, b))
        b.bids.size shouldBe 2
        List(
          (1 -> PriceLevel(10.0, 10)),
          (2 -> PriceLevel(30.0, 30))
        ).forall { case (i, p) => b.bids(i) == p } shouldBe true
      }

      "ignore update orders for a price level index not yet provided" in {
        val b = List(
          NewOrder(Bid, 1, 1, 10),
          NewOrder(Bid, 2, 2, 20)
        ).foldLeft(OrderBook(10.0, 3))((b, o) => processOrder(o, b))
        processOrder(UpdateOrder(Bid, 3, 3, 30), b) shouldBe b
      }

      "ignore delete orders for a price level index not yet provided" in {
        val b = List(
          NewOrder(Bid, 1, 1, 10),
          NewOrder(Bid, 2, 2, 20)
        ).foldLeft(OrderBook(10.0, 3))((b, o) => processOrder(o, b))
        processOrder(DeleteOrder(Bid, 3), b) shouldBe b
      }

      "ignore orders with price level index less than 1" in {
        processOrder(NewOrder(Bid, 0, 1, 10), emptyOrderBook) shouldBe emptyOrderBook
      }

      "ignore orders with price level index greater than depth" in {
        val b = OrderBook(10.0, 2)
        processOrder(NewOrder(Bid, 3, 1, 10), b) shouldBe b
      }

      "output price levels one for each line increasingly in index" in {
        val orderBook: OrderBook = OrderBook(10.0, 2, Map(
          (1 -> PriceLevel(50.0, 40)),
          (2 -> PriceLevel(40.0, 40))
        ), Map(
          (1 -> PriceLevel(60.0, 10)),
          (2 -> PriceLevel(70.0, 20))
        ))
        show(orderBook) shouldBe "50.0,40,60.0,10\n40.0,40,70.0,20".stripMargin
      }
    }
  }
}