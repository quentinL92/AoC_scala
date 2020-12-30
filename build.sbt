import sbtrelease.Version.Bump
import sbtrelease.Version.Bump.Bugfix
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
  val tmp: Option[Version] = Version(ver).map(_.withoutQualifier)
  releaseVersionBump.value match {
    case Bump.Major | Bump.Minor =>
      tmp.map(_.bump(releaseVersionBump.value).string).getOrElse(versionFormatError(ver))
    case _ =>
      tmp.map(_.string).getOrElse(versionFormatError(ver))
  }
}

releaseNextVersion := { ver =>
  Version(ver).map(_.bump(Bump.Bugfix).asSnapshot.string).getOrElse(versionFormatError(ver))
}

releaseVersionBump := Bugfix

releaseProcess := Seq[ReleaseStep](
  inquireVersions,                        // : ReleaseStep
  setReleaseVersion,                      // : ReleaseStep
  setNextVersion                         // : ReleaseStep
)