import com.raquo.laminar.api.L.*
import org.scalajs.dom
import io.laminext.websocket.WebSocket
import upickle.default.ReadWriter
import subatomic.docs.Ansi2Html
import org.scalajs.dom.HTMLElement
import scala.util.Try
import com.raquo.airstream.core.Observer

class UpdatableDiff(obs: Observable[Option[String]]):
  private var diffElement = Option.empty[HTMLElement]
  val element = pre(
    cls := "bg-black p-4 text-white text-sm w-full grow-0 overflow-scroll",
    code(
      onMountCallback(el => diffElement = Some(el.thisNode.ref)),
      onUnmountCallback(el => diffElement = None),
      obs.map { res =>
        res
          .zip(diffElement)
          .foreach: (res, el) =>
            el.innerHTML = Ansi2Html(res)
      } --> { _ => }
    )
  )
end UpdatableDiff

