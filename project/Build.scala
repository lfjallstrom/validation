import sbt._
import Keys._

object ValidationBuild extends Build {

  val librarySettings = Seq(
  	organization := "com.github.lfjallstrom",
    version := "1.0.0",
    scalaVersion := "2.10.3"
  )

  val publishSettings = Seq(
    publishTo := Some(Resolver.file("file", Path.userHome / "github" / "maven" / "releases"))
  )

  val dependencies = Seq(
  	"org.scalatest" %% "scalatest" % "1.9.2" % "test"
  )

  val sources = Seq(
    unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil,
    unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil
  )

  val project = Project("validation", file("."))
    .settings(librarySettings:_*)
    .settings(sources:_*)
    .settings(libraryDependencies ++= dependencies)
    .settings(publishSettings:_*)
}
