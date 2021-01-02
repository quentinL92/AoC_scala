import scala.util.matching.Regex

val TwoGroupsOfTwoIdenticalChar: Regex = """(?:([a-z])\1.*){2}""".r

TwoGroupsOfTwoIdenticalChar.findFirstIn("abcdffaa").isDefined