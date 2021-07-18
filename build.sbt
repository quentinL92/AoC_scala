import sbtrelease.Version.Bump.{Major, Minor, Next}
import sbtrelease.versionFormatError

name := "AoC"

scalaVersion := "2.13.4"

publish / skip := true

import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._
import sbtrelease.Version

releaseVersion := { ver =>
  val baseVersion: Option[Version] = Version(ver).map(_.withoutQualifier)
  releaseVersionBump.value match {
    case Major | Minor =>
      baseVersion.map(_.bump(releaseVersionBump.value).string).getOrElse(versionFormatError(ver))
    case _ =>
      baseVersion.map(_.string).getOrElse(versionFormatError(ver))
  }
}

releaseNextVersion := { ver =>
  println(s"Release Next Version: $ver")
  Version(ver).map(_.bump(Next).asSnapshot.string).getOrElse(versionFormatError(ver))
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
libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"