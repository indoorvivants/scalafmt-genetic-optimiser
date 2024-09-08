package shared

import upickle.default.*
import genovese.*
import java.time.Instant

case class TrainingResult(
    config: ScalafmtConfigSubset,
    formattedFile: String,
    generation: Int
)

case class JobAttributes(
    file: String,
    generations: Int,
    populationSize: Int,
    seed: Option[Long] = None
) derives ReadWriter

case class Job(
    heartbeat: Instant,
    instruction: TrainingInstruction,
    attributes: JobAttributes,
    result: Option[TrainingResult]
)

object Limits:
  lazy val MaxFileLength =
    sys.env.getOrElse("MAX_FILE_LENGTH_BYTES", "2048").toInt
  lazy val MaxPopulation  = sys.env.getOrElse("MAX_POPULATION", "100").toInt
  lazy val MaxGenerations = sys.env.getOrElse("MAX_GENERATIONS", "100").toInt
