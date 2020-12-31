import sbtrelease.Version.Bump
import sbtrelease.Version.Bump.{Bugfix, Major, Minor}
import sbtrelease.versionFormatError

name := "AoC"

scalaVersion := "2.13.4"

val enumeratumVersion = "1.6.1"

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion
)

publish / skip := true

import ReleaseTransformations._
import sbtrelease.Version

releaseVersion := { ver =>
  val baseVersion: Option[Version] = Version(ver).map(_.withoutQualifier)
  releaseVersionBump.value match {
    case Bump.Major | Bump.Minor =>
      baseVersion.map(_.bump(releaseVersionBump.value).string).getOrElse(versionFormatError(ver))
    case _ =>
      baseVersion.map(_.string).getOrElse(versionFormatError(ver))
  }
}

releaseNextVersion := { ver =>
  println(s"Release Next Version: $ver")
  Version(ver).map(_.bump(Bump.Next).asSnapshot.string).getOrElse(versionFormatError(ver))
}

releaseVersionBump := Minor

releaseProcess := Seq[ReleaseStep](
  inquireVersions,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
  pushChanges
)