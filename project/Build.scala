import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "typeSafeIds"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "org.scalaz" % "scalaz-core_2.10" % "7.0.0"
    
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
