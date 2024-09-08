package scalafmt_optimizer_cask

import shared.{*, given}
import genovese.*
import metaconfig.Conf
import org.scalafmt.config.ScalafmtConfig

import java.time.Instant
import java.util.UUID
import java.util.concurrent.{
  ConcurrentHashMap,
  Executors,
  ScheduledFuture,
  TimeUnit
}
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global as GlobalEC
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

object OptimizerServer extends cask.MainRoutes:
  override def port         = 9999
  override def host: String = "0.0.0.0"

  val scheduler = Executors.newScheduledThreadPool(1)

  @cask.get("/")
  def index() =
    cask.model.StaticResource(
      "index.html",
      getClass().getClassLoader(),
      Seq("Content-type" -> "text/html")
    )

  @cask.decorators.compress
  @cask.staticResources("/assets")
  def assets() = "assets"

  @cask.websocket("/ws/connect/:id")
  def jobUpdates(id: String): cask.WebsocketResult =
    val jobId = parseJobId(id)
    val handler = cask.WsHandler { channel =>
      var sc: Option[ScheduledFuture[?]] = None

      val actor = cask.WsActor {
        case cask.Ws.Ping(dt) =>
          cask.Ws.Pong(dt)

        case cask.Ws.Text("ping") =>
          heartbeatJob(jobId) match
            case None =>
              sc.foreach(_.cancel(true))
              channel.send(cask.Ws.Close())

            case Some(value) if value.instruction == TrainingInstruction.Halt =>
              sc.foreach(_.cancel(true))
              channel.send(cask.Ws.Close())
            case _ =>
              cask.Ws.Pong()

        case cask.Ws.Close(_, _) =>
          haltJob(jobId, "websocket connection closed")
          sc.foreach(_.cancel(true))

      }

      sc = Some(
        scheduler.scheduleAtFixedRate(
          () => sendJobUpdate(jobId, channel, () => sc.foreach(_.cancel(true))),
          1,
          1,
          TimeUnit.SECONDS
        )
      )

      actor
    }

    if jobs.containsKey(jobId) then handler else cask.Response("", 404)
  end jobUpdates

  @cask.post("/api/halt/:id")
  def halt(id: String) =
    haltJob(parseJobId(id), "cancelled from API")
      .fold(cask.Response("", statusCode = 404))(_ => cask.Response("", 204))

  @cask.post("/api/create")
  def create(req: cask.Request) =
    val attrs = upickle.default.read[JobAttributes](req.text())

    inline def error(msg: String) = cask.Response(msg, statusCode = 400)

    if attrs.file.length > Limits.MaxFileLength then
      error(
        s"File length [${attrs.file.length}] above maximum length [${Limits.MaxFileLength}]"
      )
    else if attrs.populationSize > Limits.MaxPopulation then
      error(s"Population size above maximum [${Limits.MaxPopulation}]")
    else if attrs.generations > Limits.MaxGenerations then
      error(s"Number of generations above maximum [${Limits.MaxGenerations}]")
    else if attrs.generations <= 0 || attrs.populationSize <=0 then 
      error(s"Generations and population size must both be non-negative")
    else
      val (id, job) = createJob(attrs)
      cask.Response(id.toString(), 200)
    end if
  end create

  @cask.get("/api/stats")
  def stats() =
    val result = List.newBuilder[JobSummary]

    jobs.forEach: (id, job) =>
      if job.instruction != TrainingInstruction.Halt then
        result +=
          JobSummary(id.toString, job.heartbeat.toString)

    upickle.default.writeJs(Stats("cask", result.result()))
  end stats

  scribe.Logger.root.withMinimumLevel(scribe.Level.Info).replace()

  scheduler.scheduleAtFixedRate(
    () => cleanupOldJobs(),
    0L,
    5L,
    TimeUnit.SECONDS
  )

  scheduler.scheduleAtFixedRate(
    () => reportNumberOfJobs(),
    0L,
    1L,
    TimeUnit.MINUTES
  )

  initialize()
end OptimizerServer

def parseJobId(id: String) =
  UUID.fromString(id)

def generateJobId() =
  UUID.randomUUID()

inline def absorb[T](msg: String, f: => T) =
  try f
  catch
    case NonFatal(exc) =>
      scribe.error(s"Failed to [$msg]", exc)

def sendJobUpdate(id: UUID, channel: cask.WsChannelActor, cancel: () => Unit) =
  () =>
    Option(jobs.get(id)) match
      case None =>
        absorb(
          s"closing WS connection for [$id]",
          channel.send(cask.Ws.Close())
        )
        cancel()
      case Some(job) if job.instruction == TrainingInstruction.Halt =>
        absorb(
          s"sending JobFinished for [$id]",
          channel.send(
            cask.Ws.Text(upickle.default.write(JobProgress.Finished))
          )
        )
        absorb(
          s"closing WS connection for [$id]",
          channel.send(cask.Ws.Close())
        )
        cancel()
      case Some(job) if job.result.isEmpty =>
        absorb(
          s"sending JobStarted for [$id]",
          channel.send(cask.Ws.Text(upickle.default.write(JobProgress.Started)))
        )
      case Some(job) =>
        channel.send(cask.Ws.Ping())
        job.result.foreach: result =>
          val serialised =
            Conf.printHocon(result.config.toScalafmt(base))

          val codec = ScalafmtConfig.encoder

          val diff = munit.diff
            .Diff(job.attributes.file, result.formattedFile)
            .unifiedDiff

          val configDiff =
            Conf.printHocon(
              Conf.patch(
                codec.write(base),
                codec.write(result.config.toScalafmt(base))
              )
            )

          val res = JobProgress.Result(
            config = serialised,
            formattedFile = result.formattedFile,
            fileDiff = diff,
            configDiff = configDiff,
            generation = result.generation,
            generations = job.attributes.generations
          )

          absorb(
            s"sending update for [$id]",
            channel.send(cask.Ws.Text(upickle.default.write(res)))
          )
    end match

def haltJob(id: UUID, reason: String | Null = null) =
  Option(
    jobs.computeIfPresent(
      id,
      (id, job) =>
        Option(reason).foreach: reason =>
          scribe.info(s"Job $id halted: [$reason]")
        job.copy(instruction = TrainingInstruction.Halt)
    )
  )

def heartbeatJob(id: UUID) =
  Option(
    jobs.computeIfPresent(id, (id, job) => job.copy(heartbeat = Instant.now))
  )

def createJob(attrs: JobAttributes): (UUID, Job) =
  val id = generateJobId()

  val job = Job(
    heartbeat = Instant.now(),
    instruction = TrainingInstruction.Continue,
    attributes = attrs,
    result = None
  )

  given RuntimeChecks = RuntimeChecks.None

  val trainingConfig = TrainingConfig(
    populationSize = attrs.populationSize,
    mutationRate = NormalisedFloat(0.1f),
    steps = attrs.generations,
    random =
      attrs.seed.fold(scala.util.Random())(seed => scala.util.Random(seed)),
    selection = Selection.Top(0.8)
  )

  jobs.put(id, job)

  val handler = new EventHandler:
    import TrainingEvent.*, TrainingInstruction.*
    def handle[T](t: TrainingEvent[T], data: T | Null): TrainingInstruction =
      Option(jobs.get(id)) match
        case None                                     => Halt
        case Some(value) if value.instruction == Halt => Halt
        case _ =>
          t match
            case TopSpecimen =>
              val specimen = Featureful.fromFeatures[ScalafmtConfigSubset](
                TopSpecimen.cast(data)
              )

              updateResult(id, specimen)
            case ReportFitness =>
            case EpochFinished =>
              updateJob(
                id,
                job =>
                  job.copy(result =
                    job.result.map(
                      _.copy(generation = EpochFinished.cast(data) + 1)
                    )
                  )
              )
            case TrainingFinished =>
              haltJob(id, "training finished")
            case _ =>
          end match

          Continue
    end handle
  end handler

  GlobalEC.execute: () =>
    Train(
      featureful = summon[Featureful[ScalafmtConfigSubset]],
      config = trainingConfig,
      fitness = cachedFitness(trainingConfig.random, 500)(
        Fitness(fitness(attrs.file, _))
      ),
      events = handler,
      evaluator = ParallelCollectionsEvaluator
    ).train()

  id -> job
end createJob

def updateJob(id: UUID, result: Job => Job) =
  Option(
    jobs.computeIfPresent(
      id,
      (_, job) => result(job)
    )
  )

def updateResult(id: UUID, config: ScalafmtConfigSubset) =
  updateJob(
    id,
    job =>
      val formatted =
        format(job.attributes.file, config).fold(_.toString(), identity)
      job.copy(result = job.result match
        case None =>
          Some(TrainingResult(config, formatted, 1))
        case Some(value) =>
          Some(value.copy(config = config, formattedFile = formatted))
      )
  )

val jobs = ConcurrentHashMap[UUID, Job]

def cleanupOldJobs() =
  val result = jobs.values.removeIf: job =>
    Instant.now().minusSeconds(60L).isAfter(job.heartbeat)

  if result then scribe.info("Some jobs were removed because they were stale")
  else scribe.debug("No stale jobs removed")

def reportNumberOfJobs() =
  val nJobs = jobs.size()
  if nJobs > 0 then scribe.info(s"Number of active jobs: $nJobs")
