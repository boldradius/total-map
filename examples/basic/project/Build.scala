import sbt._
import Keys._

object ApplicationBuild extends Build {
  val main = Project("basic", file(".")).settings(
  scalaVersion := "2.11.4",
  libraryDependencies ++= Seq("com.boldradius" %% "total-map" % "0.2.1-SNAPSHOT"),
  scalacOptions ++= Seq("-feature", "-deprecation"))
}
