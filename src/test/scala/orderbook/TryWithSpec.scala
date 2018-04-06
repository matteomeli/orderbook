package quantemplate.exercise

import java.io.Closeable
import org.scalatest.{Matchers, WordSpec}
import scala.util.{Failure, Success}

class TryWithSpec extends WordSpec with Matchers {
  val acquireException = new RuntimeException
  val consumeException = new RuntimeException
  val closeException = new RuntimeException
  val acquireError = new OutOfMemoryError
  val consumeError = new OutOfMemoryError
  val closeError = new OutOfMemoryError

  val goodResource: Closeable = new Closeable {
    override def toString: String = "A good resource"
    def close(): Unit = {}
  }

  val closeExcpetionResource: Closeable = new Closeable {
    override def toString: String = "A bad resource (exception)"
    def close(): Unit = throw closeException
  }

  val closeErrorResource: Closeable = new Closeable {
    override def toString: String = "A bad resource (error)"
    def close(): Unit = throw closeError
  }

  "TryWith" should {
    "catch exceptions while acquiring the resource" in {
      TryWith(throw acquireException)(println) shouldBe Failure(acquireException)
    }

    "catch exceptions while consuming the resource" in {
      TryWith(goodResource) {
        _ => throw consumeException
      } shouldBe Failure(consumeException)
    }

    "catch exceptions while closing the resource" in {
      TryWith(closeExcpetionResource)(_.toString) shouldBe Failure(closeException)
    }

    "add closing exceptions as suppressed to resource consumer function exception if any" in {
      val result = TryWith(closeExcpetionResource) {
        _ => throw consumeException
      }

      result shouldBe Failure(consumeException)
      val Failure(resultException) = result
      resultException.getSuppressed shouldBe Array(closeException)
    }

    "propagate errors while acquiring the resource" in {
      intercept[OutOfMemoryError] {
        TryWith(throw acquireError)(println)
      } shouldBe acquireError
    }

    "propagate errors while consuming the resource" in {
      intercept[OutOfMemoryError] {
        TryWith(goodResource) {
          _ => throw consumeError
        } shouldBe consumeError
      }
    }

    "propagate errors while closing the resource" in {
      intercept[OutOfMemoryError] {
        TryWith(closeErrorResource)(_.toString)
      } shouldBe closeError
    }

    "contain the value if no exceptions or error are encountered" in {
      TryWith(goodResource)(_.toString) shouldBe Success("A good resource")
    }
  }

}