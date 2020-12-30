name := "AoC"

version := "0.1"

scalaVersion := "2.13.4"

val enumeratumVersion = "1.6.1"

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion
)

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,              // : ReleaseStep
  inquireVersions,                        // : ReleaseStep
  setReleaseVersion,                      // : ReleaseStep
  setNextVersion                         // : ReleaseStep
)