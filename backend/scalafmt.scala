package shared 

import genovese.*
import org.scalafmt.Scalafmt
import org.scalafmt.config.*
import org.scalafmt.config.Docstrings.*
import org.scalafmt.config.Indents.FewerBraces

import java.util.concurrent.ConcurrentHashMap
import scala.util.Random

given Featureful[Oneline] = Featureful.derive[Oneline](FieldConfig.None)
given Featureful[Wrap]    = Featureful.derive[Wrap](FieldConfig.None)
given Featureful[BlankFirstLine] =
  Featureful.derive[BlankFirstLine](FieldConfig.None)
given Featureful[Style] = Featureful.derive[Style](FieldConfig.None)
given Featureful[Docstrings] = Featureful.derive[Docstrings](
  FieldConfig(Map("wrapMaxColumn" -> Feature.IntRange(0, 1)))
)

given commentsWrap: Featureful[Comments.Wrap] =
  Featureful.derived[Comments.Wrap]
given Featureful[Comments] = Featureful.derived[Comments]
given Featureful[OptIn]    = Featureful.derived[OptIn]

given Featureful[BinPack.Site]        = Featureful.derived[BinPack.Site]
given Featureful[BinPack.ParentCtors] = Featureful.derived[BinPack.ParentCtors]
given Featureful[BinPack] = Featureful.derive[BinPack](
  FieldConfig(
    Map(
      "literalsMinArgCount" -> Feature.IntRange(1, 8),
      "literalsInclude"     -> Feature.StringCategory(List(".*")),
      "literalsExclude" -> Feature.StringCategory(List("String", "Term.Name"))
    )
  )
)

given Featureful[FewerBraces] = Featureful.derived[FewerBraces]
given Featureful[Indents.RelativeToLhs] =
  Featureful.derived[Indents.RelativeToLhs]
given Featureful[Indents] = Featureful.derive[Indents](
  FieldConfig(
    Map(
      "main"                         -> Feature.IntCategory(List(0, 2, 4)),
      "significant"                  -> Feature.IntCategory(List(0, 2, 4)),
      "callSite"                     -> Feature.IntCategory(List(0, 2, 4)),
      "ctrlSite"                     -> Feature.IntCategory(List(0, 2, 4)),
      "binPackCallSite"              -> Feature.IntCategory(List(0, 2, 4)),
      "defnSite"                     -> Feature.IntCategory(List(0, 2, 4)),
      "caseSite"                     -> Feature.IntCategory(List(0, 2, 4)),
      "matchSite"                    -> Feature.IntCategory(List(0, 2, 4)),
      "ctorSite"                     -> Feature.IntCategory(List(0, 2, 4)),
      "extraBeforeOpenParenDefnSite" -> Feature.IntCategory(List(0, 2, 4)),
      "binPackDefnSite"              -> Feature.IntCategory(List(0, 2, 4)),
      "afterInfixSite"               -> Feature.IntCategory(List(0, 2, 4)),
      "withSiteRelativeToExtends"    -> Feature.IntCategory(List(0, 2, 4)),
      "extendSite"                   -> Feature.IntCategory(List(0, 2, 4)),
      "commaSiteRelativeToExtends"   -> Feature.IntCategory(List(0, 2, 4))
    )
  )
)

case class DialectOptions(dialectName: String)

given Featureful[DialectOptions] = Featureful.derive(
  FieldConfig(
    Map(
      "dialectName" -> Feature.StringCategory(List("scala213", "scala3"))
    )
  )
)

type ScalafmtConfigSubset =
  (Docstrings, Comments, Indents, BinPack, OptIn, DialectOptions)

given Featureful[ScalafmtConfigSubset] =
  Featureful.deriveTuple[ScalafmtConfigSubset]

extension (cfg: ScalafmtConfigSubset)
  def toScalafmt(base: ScalafmtConfig): ScalafmtConfig =
    base
      .copy(
        docstrings = cfg._1,
        comments = cfg._2,
        indent = cfg._3,
        binPack = cfg._4,
        optIn = cfg._5
      )
      .withDialect(dialect)

  def dialect = cfg._6.dialectName match
    case "scala213" => scala.meta.dialects.Scala213
    case "scala3"   => scala.meta.dialects.Scala3
end extension

val base = ScalafmtConfig()

enum CacheEvent:
  case Hit, Miss, Eviction

inline def cachedFitness[T](
    random: Random,
    size: Int,
    events: CacheEvent => Unit = _ => ()
)(
    fitness: Fitness[T]
): Fitness[T] =
  val cache = ConcurrentHashMap[T, NormalisedFloat]()
  import CacheEvent.*
  Fitness: t =>
    if cache.size() >= size then
      // Do a half-Thanos
      cache.values.removeIf(_ => random.nextFloat() <= 0.25f)
      events(Eviction)
    var missed = false
    val result = cache.computeIfAbsent(
      t,
      _ =>
        missed = true
        fitness(t)
    )

    if missed then events(Miss)
    else events(Hit)

    result
end cachedFitness

def format(text: String, cfg: ScalafmtConfigSubset): Either[Throwable, String] =
  Scalafmt
    .format(
      text.trim,
      style = cfg.toScalafmt(base)
    )
    .toEither

def fitness(text: String, cfg: ScalafmtConfigSubset) =
  import com.github.vickumar1981.stringdistance.*
  import com.github.vickumar1981.stringdistance.StringDistance.*
  import com.github.vickumar1981.stringdistance.impl.ConstantGap

  Scalafmt
    .format(
      text.trim,
      style = cfg.toScalafmt(base)
    )
    .toEither
    .fold(
      exc => NormalisedFloat.ZERO,
      formatted =>
        NormalisedFloat.applyUnsafe(
          math
            .abs(NeedlemanWunsch.score(formatted, text, ConstantGap()))
            .toFloat
            .max(0.0f)
            .min(1.0f)
        )
    )

end fitness
