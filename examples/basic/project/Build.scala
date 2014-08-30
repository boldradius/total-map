import sbt._
import Keys._

object ApplicationBuild extends Build {
  val main = Project("basic", file(".")).settings(
  scalaVersion := "2.11.2",
  libraryDependencies ++= Seq("com.boldradius" %% "total-map" % "0.1.8"),
  scalacOptions ++= Seq("-feature", "-deprecation"))
}
