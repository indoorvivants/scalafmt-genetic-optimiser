import shared.*

import com.raquo.laminar.api.L.*
import io.laminext.websocket.WebSocket

import upickle.default.ReadWriter
import com.raquo.airstream.core.Signal

case class Result(
    config: String,
    formattedFile: String,
    fileDiff: String,
    configDiff: String,
    generation: Int,
    generations: Int
) derives ReadWriter

case class JobAttributes(
    file: String,
    generations: Int,
    populationSize: Int,
    seed: Long
) derives ReadWriter

@main def hello =
  val id          = Var[Option[String]](None)
  val result      = Var[Option[JobProgress]](None)
  val serverStats = Var[Option[Stats]](None)

  val jobAttributes = Var(Option.empty[JobAttributes])

  val text = Var(SaveState.readString(SaveState.TEXT_KEY, DEFAULT_TEXT))

  val generations = Var(
    SaveState.readString(SaveState.GENERATIONS_KEY).toIntOption.getOrElse(100)
  )
  val populationSize = Var(
    SaveState.readString(SaveState.POPULATION_KEY).toIntOption.getOrElse(100)
  )
  val seed = Var(
    SaveState.readString(SaveState.SEED_KEY).toLongOption.getOrElse(8008L)
  )

  val fileDiff = UpdatableDiff(result.signal.map(_.collect {
    case jr: JobProgress.Result => jr.fileDiff
  }))
  val configDiff = UpdatableDiff(result.signal.map(_.collect {
    case jr: JobProgress.Result => jr.configDiff
  }))

  val error = Var(Option.empty[String])

  val updateJob =
    id.signal.flatMapSwitch:
      case None => Signal.fromValue(None)
      case Some(id) =>
        val client =
          WebSocket.path(s"/ws/connect/$id").string.build(managed = false)

        Signal
          .fromValue(client.reconnectNow())
          .flatMapSwitch: _ =>
            client.received
              .tapEach(_ => client.sendOne("ping"))
              .map(upickle.default.read[JobProgress](_))
              .map(Some(_))
              .startWith(None)

  val updateStats =
    EventStream
      .periodic(5000)
      .flatMapSwitch: _ =>
        FetchStream.get("/api/stats").map(upickle.default.read[Stats](_))
    --> serverStats.someWriter

  val cancelButton =
    button(
      "Cancel",
      cls <-- id.signal.map(_.fold("invisible")(_ => "visible")),
      onClick.preventDefault.flatMapTo(
        FetchStream.post(s"/api/halt/${id.now().getOrElse("")}")
      ) --> { _ => },
      buttonStyle("rose-600")
    )

  inline def buttonStyle(color: String) =
    cls := s"block border-2 border-2 border-r-8 border-$color p-2 hover:bg-$color"

  val trainingStatus =
    child <-- result.signal
      .withCurrentValueOf(id.signal)
      .map: (res, id) =>
        res match
          case None =>
            id match
              case None => p(" ")
              case Some(value) =>
                p("Job submitted, waiting for first update...")

          case Some(res: JobProgress.Result) =>
            p(
              s"Training... generation ${res.generation} out of ${res.generations}"
            )
          case Some(JobProgress.Started) =>
            p("Job started...")
          case Some(JobProgress.Finished) =>
            p("Job finished")

  val errorStatus =
    child.maybe <-- error.signal.map(
      _.map(msg => p(cls := "text-rose-700", msg))
    )

  val attrsSignal =
    Signal.combineWithFn(
      text.signal,
      generations.signal,
      populationSize.signal,
      seed.signal
    )(JobAttributes.apply)

  val resetError = attrsSignal --> { _ => error.set(None) }

  val submitForm = form(
    action := "#",
    div(
      cls := "flex gap-4 justify-start",
      div(
        cls := "w-6/12",
        h2("Scala code:", cls := "text-xl font-bold"),
        textArea(
          cls  := "p-2 border-2 border-grey-600 rounded-md",
          rows := 11,
          cols := 60,
          onInput.mapToValue --> text,
          value <-- text.signal
        )
      ),
      div(
        cls := "flex flex-col gap-2",
        p(
          p(strong("Generations: ")),
          input(
            tpe := "text",
            cls := "p-2 border-2 border-grey-600 rounded-md",
            value <-- generations.signal.map(_.toString),
            onInput.mapToValue --> generations.writer
              .filter(_ > 0)
              .contramap[String](_.toInt)
          )
        ),
        p(
          cls := "leading-6",
          p(strong("Population size: ")),
          input(
            tpe := "text",
            cls := "p-2 border-2 border-grey-600 rounded-md",
            controlled(
              value <-- populationSize.signal.map(_.toString),
              onInput.mapToValue --> populationSize.writer
                .filter(_ > 0)
                .contramap[String](_.toInt)
            )
          )
        ),
        p(
          cls := "leading-6",
          p(strong("Random seed:")),
          input(
            tpe         := "text",
            placeholder := "8008",
            cls         := "p-2 border-2 border-grey-600 rounded-md",
            controlled(
              value <-- seed.signal.map(_.toString),
              onInput.mapToValue --> seed.writer
                .filter(_ > 0)
                .contramap[String](_.toLong)
            )
          )
        ),
        div(
          cls := "flex gap-2 w-full",
          button(tpe := "submit", "Submit", buttonStyle("amber-600")),
          cancelButton
        ),
        errorStatus,
        trainingStatus
      ),
      div(child.maybe <-- serverStats.signal.map(_.map(renderStats)))
    ),
    onSubmit.preventDefault.flatMapTo(
      jobAttributes.now() match
        case None => EventStream.fromValue(None)
        case Some(value) =>
          id.set(None)
          result.set(None)
          FetchStream.raw
            .post("/api/create", _.body(upickle.default.write(value)))
            .combineWith(
              FetchStream.raw.post(s"/api/halt/${id.now().getOrElse("")}")
            )
            .map(_._1)
            .flatMapSwitch: response =>
              if response.ok then
                error.set(None)
                EventStream.fromJsPromise(response.text()).map(Some(_))
              else
                response.text().`then`(txt => error.set(Some(txt)))
                EventStream.fromValue(None)
    ) --> id
  )

  val basicLink =
    cls := "text-indigo-500 underline hover:no-underline"

  val app =
    div(
      div(
        cls := "my-4 flex flex-col gap-2",
        h1(
          cls := "text-4xl underline",
          "Scalafmt config genetic optimiser"
        ),
        p(
          "This is a toy application exploring the use of Genetic Algorithms " +
            "to find an optimal Scalafmt config that introduces minimal amount of changes to a given file"
        ),
        div(
          cls := "flex gap-2",
          a(
            basicLink,
            href := "https://github.com/indoorvivants/scalafmt-genetic-optimiser",
            "Github repository"
          ),
          a(
            basicLink,
            href := "http://twitter.com/velvetbaldmime",
            "Author on Twitter"
          ),
          a(
            basicLink,
            href := "https://blog.indoorvivants.com/2024-09-27-scalafmt-genetic-optimiser",
            "Blog post about this"
          )
        )
      ),
      submitForm,
      updateJob --> result,
      resetError,
      updateStats,
      attrsSignal --> jobAttributes.someWriter,
      text.signal --> SaveState.saveString(SaveState.TEXT_KEY),
      generations.signal.map(_.toString) --> SaveState.saveString(
        SaveState.GENERATIONS_KEY
      ),
      populationSize.signal.map(_.toString) --> SaveState.saveString(
        SaveState.POPULATION_KEY
      ),
      div(
        visibility <-- result.signal.map(_.fold("hidden")(_ => "visible")),
        cls := "flex gap-4 w-full",
        div(
          cls := "w-6/12",
          p(cls := "text-lg font-bold", "File diff:"),
          fileDiff.element
        ),
        div(
          cls := "w-6/12",
          p(cls := "text-lg font-bold", "Scalafmt configuration:"),
          configDiff.element
        )
      ),
      id.signal.map("id=" + _.toString).debugLog() --> Observer.empty,
      result.signal.map("result=" + _.toString()).debugLog() --> Observer.empty
    )

  renderOnDomContentLoaded(
    org.scalajs.dom.document.getElementById("hello"),
    app
  )
end hello

def renderStats(stats: Stats) = div(
  p(strong("Server: "), stats.serverType),
  p(
    if stats.jobs.nonEmpty then strong("Active jobs") else em("No active jobs")
  ),
  ul(
    cls := "text-xs",
    stats.jobs.map: job =>
      li(cls := "ml-2", strong(job.id.toString), " : ", job.hearbeat)
  )
)

val DEFAULT_TEXT =
  """
  abstract class MemberDefTraverser extends Traverser {
    def onMember(defn: MemberDef): Unit

    private var depth: Int = 0
    private def lower[T](body: => T): T = {
      depth += 1
      try body finally depth -= 1
    }
    def currentDepth = depth

    /** Prune this tree and all trees beneath it. Can be overridden. */
    def prune(md: MemberDef): Boolean = (
         md.mods.isSynthetic
      || md.mods.isParamAccessor
      || nme.isConstructorName(md.name)
      || (md.name containsName nme.ANON_CLASS_NAME)
    )

    override def traverse(t: Tree): Unit = t match {
      case md: MemberDef if prune(md) =>
      case md @ PackageDef(_, stats)  => traverseTrees(stats)
      case md: ImplDef                => onMember(md) ; lower(traverseTrees(md.impl.body))
      case md: ValOrDefDef            => onMember(md) ; lower(traverse(md.rhs))
      case _                          => super.traverse(t)
    }
  }
"""
