import com.raquo.laminar.api.L.*
import org.scalajs.dom
import scala.util.Try
import com.raquo.airstream.core.Observer

object SaveState:
  val TEXT_KEY        = "scalafmt-genetic-optimiser-text"
  val POPULATION_KEY  = "scalafmt-genetic-optimiser-population"
  val GENERATIONS_KEY = "scalafmt-genetic-optimiser-generations"
  val SEED_KEY        = "scalafmt-genetic-optimiser-seed"

  import upickle.default.*

  def readFrom[T: ReadWriter](key: String, default: T) =
    dom.window.localStorage.getItem(key) match
      case null => default
      case other =>
        Try(read[T](other)).getOrElse(default)

  def save[T: ReadWriter](key: String, signal: Signal[T]) =
    signal.map(upickle.default.write(_)) --> { written =>
      dom.window.localStorage.setItem(key, written)
    }

  def readString(key: String, default: String = ""): String =
    dom.window.localStorage.getItem(key) match
      case null  => default
      case other => other

  def saveString(key: String): Observer[String] =
    Observer[String](written => dom.window.localStorage.setItem(key, written))
end SaveState

