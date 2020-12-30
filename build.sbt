name := "AoC"

version := "0.1"

scalaVersion := "2.13.4"

val enumeratumVersion = "1.6.1"

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion
)

publishTo := Some(Resolver.file("file", new File("D:\\Temp")))

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  inquireVersions,                        // : ReleaseStep
  setReleaseVersion,                      // : ReleaseStep
  setNextVersion                         // : ReleaseStep
)