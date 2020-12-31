import sbtrelease.Version.Bump
import sbtrelease.Version.Bump.{Bugfix, Major, Minor}
import sbtrelease.versionFormatError

name := "AoC"

scalaVersion := "2.13.4"

val enumeratumVersion = "1.6.1"

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion
)

//publishTo := Some(Resolver.file("file", new File("/Users/al/tmp")))

publish / skip := true

import ReleaseTransformations._
import sbtrelease.Version

releaseVersion := { ver =>
  println()
  println(s"Release Version start $ver")
  println()
  val tmp: Option[Version] = Version(ver).map(_.withoutQualifier)
  println(s"Release Version withoutQualifier ${tmp.get.subversions}")
  val res = releaseVersionBump.value match {
    case Bump.Major | Bump.Minor =>
      tmp.map(_.bump(releaseVersionBump.value).string).getOrElse(versionFormatError(ver))
    case _ =>
      tmp.map(_.string).getOrElse(versionFormatError(ver))
  }
  println(s"Release Version end $res")
  res
}

releaseNextVersion := { ver =>
  println(s"Release Next Version: $ver")
  Version(ver).map(_.bump(Bump.Next).asSnapshot.string).getOrElse(versionFormatError(ver))
}

releaseVersionBump := Bugfix

releaseProcess := Seq[ReleaseStep](
  inquireVersions,                        // : ReleaseStep
  setReleaseVersion,                      // : ReleaseStep
  setNextVersion                         // : ReleaseStep
)