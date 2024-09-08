package scalafmt_optimizer_http4s

import shared.{*, given}
import cats.data.Kleisli
import cats.effect.*
import cats.effect.std.{Dispatcher, Queue, Supervisor}
import com.comcast.ip4s.*
import metaconfig.Conf
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame
import org.http4s.{EntityEncoder, Response, *}
import org.scalafmt.config.ScalafmtConfig
import scribe.Scribe

import genovese.*

import java.util.UUID

import headers.`Content-Type`
import concurrent.duration.*

object OptimizerServer extends IOApp.Simple:

  import org.http4s.implicits.*

  override def run: IO[Unit] =
    JobManager.create
      .map(routes(_))
      .flatMap: routes =>
        EmberServerBuilder
          .default[IO]
          .withPort(port"9999")
          .withHost(host"0.0.0.0")
          .withHttpWebSocketApp(wbs =>
            handleErrors(scribe.cats.io, routes(wbs).orNotFound)
          )
          .build
      .useForever

  def handleErrors(logger: Scribe[IO], routes: HttpApp[IO]): HttpApp[IO] =
    import cats.syntax.all.*
    routes.onError { exc =>
      Kleisli(request =>
        logger.error("Request failed", request.toString, exc.fillInStackTrace())
      )
    }
end OptimizerServer

extension [A](r: IO[Response[IO]])
  def json =
    r.map(_.withContentType(`Content-Type`(MediaType.application.json)))

def sendJobUpdate(
    q: Queue[IO, WebSocketFrame],
    manager: JobManager,
    id: UUID,
    interrupt: IO[Unit]
) =
  def sendProgress(progress: JobProgress) =
    q.offer(
      WebSocketFrame
        .Text(upickle.default.write(progress))
    )

  manager
    .get(id)
    .flatMap:
      case None =>
        q.offer(WebSocketFrame.Close())
      case Some(job) if job.instruction == TrainingInstruction.Halt =>
        interrupt *>
          sendProgress(JobProgress.Finished) *>
          q.offer(WebSocketFrame.Close())
      case Some(job) =>
        job.result match
          case None =>
            sendProgress(JobProgress.Started)
          case Some(result) =>
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

            q.offer(WebSocketFrame.Ping()) *>
              sendProgress(res)
end sendJobUpdate

def routes(
    manager: JobManager
)(wbs: WebSocketBuilder2[IO]): HttpRoutes[IO] =
  HttpRoutes.of[IO]:
    case GET -> Root / "api" / "stats" =>
      manager.getAll.flatMap: mp =>
        val active = mp.filter(_._2.instruction != TrainingInstruction.Halt)
        val stats = Stats(
          "http4s",
          active.toList
            .sortBy(_._2.heartbeat)
            .reverse
            .map((id, j) => JobSummary(id.toString, j.heartbeat.toString))
        )

        Ok(upickle.default.write(stats)).json

    case GET -> Root / "ws" / "connect" / UUIDVar(uuid) =>
      manager.getAll
        .map(_.get(uuid))
        .flatMap:
          case None => NotFound(s"Job $uuid not found")
          case Some(job) =>
            IO.deferred[Either[Throwable, Unit]]
              .product(Queue.bounded[IO, WebSocketFrame](1024))
              .flatMap: (latch, q) =>

                val repeatedUpdates = fs2.Stream
                  .repeatEval(
                    sendJobUpdate(
                      q,
                      manager,
                      uuid,
                      latch.complete(Right(())).void
                    )
                  )
                  .metered(1.second)
                  .interruptWhen(latch)

                val send = fs2.Stream
                  .fromQueueUnterminated(q)
                  .concurrently(repeatedUpdates)

                val receive: fs2.Pipe[IO, WebSocketFrame, Unit] =
                  _.evalTap:
                    case WebSocketFrame.Text("ping", _) =>
                      manager.heartbeatJob(uuid).void
                    case WebSocketFrame.Close(_) => latch.complete(Right(()))
                    case _                       => IO.unit
                  .drain

                wbs.withFilterPingPongs(false).build(send, receive)

    case req @ POST -> Root / "api" / "halt" / UUIDVar(id) =>
      manager.haltJob(id, "halted from API") *> Ok()

    case req @ POST -> Root / "api" / "create" =>
      req.bodyText.compile.string
        .map(upickle.default.read[JobAttributes](_))
        .flatMap: attrs =>
          inline def error(msg: String) = BadRequest(msg)

          if attrs.file.length > Limits.MaxFileLength then
            error(
              s"File length [${attrs.file.length}] above maximum length [${Limits.MaxFileLength}]"
            )
          else if attrs.populationSize > Limits.MaxPopulation then
            error(s"Population size above maximum [${Limits.MaxPopulation}]")
          else if attrs.generations > Limits.MaxGenerations then
            error(
              s"Number of generations above maximum [${Limits.MaxGenerations}]"
            )
          else if attrs.generations <= 0 || attrs.populationSize <=0 then 
            error(s"Generations and population size must both be non-negative")
          else manager.createJob(attrs).flatMap((id, _) => Ok(id.toString()))
          end if

    case request @ GET -> Root =>
      StaticFile
        .fromResource("/index.html", Some(request))
        .getOrElseF(NotFound())

    case request @ GET -> Root / "assets" / path
        if List(".js", ".css").exists(path.endsWith) =>
      StaticFile
        .fromResource("/assets/" + path, Some(request))
        .getOrElseF(NotFound())
end routes

object JobManager:
  import cats.syntax.all.*
  def create =
    (IO.ref(Map.empty).toResource, Supervisor[IO], Dispatcher[IO])
      .mapN(JobManager(_, _, _))

class JobManager(
    state: Ref[IO, Map[UUID, Job]],
    supervisor: Supervisor[IO],
    dispatcher: Dispatcher[IO]
):
  def cleanupOldJobs =
    IO.realTimeInstant.flatMap: now =>
      state.update(
        _.filterNot((_, job) => now.minusSeconds(60L).isAfter(job.heartbeat))
      )

  def reportNumberOfJobs =
    state.get.flatTap: jobs =>
      if jobs.size > 0 then
        scribe.cats.io.info(s"Number of active jobs: ${jobs.size}")
      else IO.unit

  def heartbeatJob(id: UUID) =
    IO.realTimeInstant.flatMap: now =>
      state.modify: jobs =>
        jobs.get(id) match
          case None => jobs -> None
          case Some(value) =>
            jobs.updated(id, value.copy(heartbeat = now)) -> Some(value)

  def generateJobId() = IO(UUID.randomUUID())

  def getAll        = state.get
  def get(id: UUID) = getAll.map(_.get(id))

  val evaluator = IOParallelEvaluator(dispatcher)

  def createJob(
      attrs: JobAttributes
  ): IO[(UUID, Job)] =

    import genovese.*
    given RuntimeChecks = RuntimeChecks.None

    for
      id  <- generateJobId()
      now <- IO.realTimeInstant

      job = Job(
        heartbeat = now,
        instruction = TrainingInstruction.Continue,
        attributes = attrs,
        result = None
      )

      trainingConfig = TrainingConfig(
        populationSize = attrs.populationSize,
        mutationRate = NormalisedFloat(0.1f),
        steps = attrs.generations,
        random =
          attrs.seed.fold(scala.util.Random())(seed => scala.util.Random(seed)),
        selection = Selection.Top(0.8)
      )

      _ <- state.update(_.updated(id, job))

      training = IO:
        Train(
          featureful = summon[Featureful[ScalafmtConfigSubset]],
          config = trainingConfig,
          fitness = cachedFitness(trainingConfig.random, 500)(
            Fitness(fitness(attrs.file, _))
          ),
          events = handler(id),
          evaluator = evaluator
        ).train()

      fib <- supervisor.supervise(training)
    yield id -> job
    end for
  end createJob

  def handler(id: UUID)(using RuntimeChecks) =
    val getJob = state.get.map(_.get(id))
    new EventHandler:
      import TrainingEvent.*, TrainingInstruction.*

      def handle[T](
          t: TrainingEvent[T],
          data: T | Null
      ): TrainingInstruction =
        dispatcher.unsafeRunSync(getJob) match
          case None                                     => Halt
          case Some(value) if value.instruction == Halt => Halt
          case _ =>
            t match
              case TopSpecimen =>
                val specimen = Featureful.fromFeatures[ScalafmtConfigSubset](
                  TopSpecimen.cast(data)
                )

                dispatcher.unsafeRunSync(updateResult(id, specimen))
              case ReportFitness =>
              case EpochFinished =>
                dispatcher.unsafeRunSync(
                  updateJob(
                    id,
                    job =>
                      job.copy(result =
                        job.result.map(
                          _.copy(generation = EpochFinished.cast(data) + 1)
                        )
                      )
                  )
                )
              case TrainingFinished =>
                dispatcher.unsafeRunSync(haltJob(id, "training finished"))
              case _ =>
            end match

            Continue
      end handle
    end new
  end handler

  class IOParallelEvaluator(dispatcher: Dispatcher[IO]) extends Evaluator:
    import cats.effect.syntax.all.*
    override def evaluate(
        population: Population,
        fitness: Vec => NormalisedFloat
    ): Evaluated =
      dispatcher.unsafeRunSync(
        population.toList
          .parTraverseN(4)(vec => IO(fitness(vec)))
          .map(IArray.from)
          .map(scores => Evaluated(population.zip(scores)))
      )
  end IOParallelEvaluator

  def updateJob(id: UUID, f: Job => Job) =
    state.update(_.updatedWith(id)(_.map(f)))

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

  def haltJob(id: UUID, message: String): IO[Unit] =
    state
      .modify: jobs =>
        jobs.get(id) match
          case None => jobs -> IO.unit
          case Some(value) =>
            jobs.updated(
              id,
              value.copy(instruction = TrainingInstruction.Halt)
            ) -> scribe.cats.io
              .info(s"Job [$id] halted: [$message]")
      .flatten

end JobManager
