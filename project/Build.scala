import sbt._
import sbt.Keys._

object MyBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    name := "ivth",
    version := "0.0.1",
    scalaVersion := "2.10.0-RC5",

    scalacOptions ++= Seq(
      "-Yinline-warnings",
      "-deprecation",
      "-optimize",
      "-unchecked"
    ),

    libraryDependencies ++= Seq(
      "org.spire-math" % "spire_2.10.0-RC5" % "0.3.0-M6"
    )
  )

  lazy val root = Project("ivth", file("."))
}
