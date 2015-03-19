import sbt._
import Keys._

object ApplicationBuild extends Build {
  val main = Project("basic", file(".")).settings(
  scalaVersion := "2.11.4",
  resolvers += Resolver.bintrayRepo("boldradiussolutions", "maven"),
  libraryDependencies ++= Seq("com.boldradius" %% "total-map" % "0.2.3-SNAPSHOT"),
  scalacOptions ++= Seq("-feature", "-deprecation"))
}
