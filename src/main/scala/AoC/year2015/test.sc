import scala.util.matching.Regex

val s = "vizslas: 2, pomeranians: 1, perfumes: 7"

def fieldRegex(fieldName: String): Regex = s"(?:.*)$fieldName: (\\d+)(?:.*)".r
val PerfumeRegex = fieldRegex("perfumes")

s match {
  case PerfumeRegex(quantity) => Some(quantity.toInt)
  case _ => None
}