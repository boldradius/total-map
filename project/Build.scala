import sbt._
import Keys._

object ApplicationBuild extends Build {
  val main = Project("total-map", file(".")).settings(
  version := "0.1.8",
  organization := "com.boldradius",
  scalaVersion := "2.11.2"
  )
}
