import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    version := "0.1",
    scalaVersion := "2.10.1",
    scalacOptions ++= Seq("-deprecation", "-feature")
  )
}

object Dependencies {
  val reflect = (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
  val scalatest = "org.scalatest" % "scalatest_2.10" % "2.0.M5b"
  val kiama = "com.googlecode.kiama" % "kiama_2.10" % "1.4.0"
}

object TypeCheckLibBuild extends Build {
  import BuildSettings._
  import Dependencies._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings
  ) aggregate(macros, core, examples)

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings :+ (libraryDependencies <+= reflect)
  )

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings ++ Seq(
      name := "typechecklib",
      mainClass := None,
      parallelExecution in Test := false,
      libraryDependencies <+= reflect,
      libraryDependencies += scalatest,
      libraryDependencies += kiama
    )
  ) dependsOn(macros)

  lazy val examples: Project = Project(
    "examples",
    file("examples"),
    settings = buildSettings
  ) dependsOn(core)
}
