package orderbook

import scala.util.{Failure, Success, Try}

sealed trait OrderSide
case object Bid extends OrderSide
case object Ask extends OrderSide

sealed trait Order {
  val side: OrderSide
  val index: Int
}
case class NewOrder(side: OrderSide, index: Int, ticks: Int, quantity: Int) extends Order
case class UpdateOrder(side: OrderSide, index: Int, ticks: Int, quantity: Int) extends Order
case class DeleteOrder(side: OrderSide, index: Int) extends Order

object Order {
  implicit val parseOrderSide = Parser[OrderSide](s =>
    s match {
      case "B" => Success(Bid)
      case "A" => Success(Ask)
      case _ => Failure(new IllegalArgumentException(s"Cannot parse '$s' into an OrderSide"))
    }
  )

  def parse(s: String): Try[Order] = {
    s.trim.split(' ').toList match {
      case c1 :: c2 :: c3 :: c4 :: c5 :: _ if c1 == "N" || c1 == "U" =>
        for {
          side <- Parser.parse[OrderSide](c2)
          index <- Parser.parse[Int](c3)
          ticks <- Parser.parse[Int](c4)
          quantity <- Parser.parse[Int](c5)
        } yield if (c1 == "N") NewOrder(side, index, ticks, quantity)
          else UpdateOrder(side, index, ticks, quantity)
      case c1 :: c2 :: c3 :: _ if c1 == "D" =>
        for {
          side <- Parser.parse[OrderSide](c2)
          index <- Parser.parse[Int](c3)
        } yield DeleteOrder(side, index)
      case _ => Failure(new IllegalArgumentException(s"Cannot parse line '$s' into an Order"))
    }
  }
}