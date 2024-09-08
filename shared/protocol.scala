//> using scala "3.5.0"

//> using dependency "com.lihaoyi::upickle::4.0.1"

package shared

import upickle.default.*

enum JobProgress derives ReadWriter:
  case Started
  case Finished
  case Result(
    config: String,
    formattedFile: String,
    fileDiff: String,
    configDiff: String,
    generation: Int,
    generations: Int
) 

case class JobSummary(id: String, hearbeat: String) derives ReadWriter

case class Stats(
  serverType: String,
  jobs: List[JobSummary]
) derives ReadWriter
