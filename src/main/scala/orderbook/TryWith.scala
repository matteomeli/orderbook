package orderbook

import java.io.Closeable
import scala.util.control.NonFatal
import scala.util.{Failure, Try}

object TryWith {
  def apply[C <: Closeable, R](resource: => C)(f: C => R): Try[R] =
    Try(resource).flatMap { closeable =>
      try {
        val result = f(closeable)
        Try(closeable.close()).map(_ => result)
      }
      catch {
        // Handle exception in function f execution
        case NonFatal(ef) =>
          try {
            closeable.close()
            Failure(ef)
          }
          catch {
            // Handle exception in close()
            case NonFatal(ec) =>
              ef.addSuppressed(ec)
              Failure(ef)
          }
      }
    }
}