name := "typechecklib"

version := "0.1"

// Compiler settings

scalaVersion := "2.10.1"

scalacOptions ++= Seq( "-deprecation", "-feature" )

// No main class (library)

mainClass := None

// Library dependencies

libraryDependencies ++= Seq( "com.googlecode.kiama" % "kiama_2.10" % "1.4.0" ,
                             "org.scalatest" % "scalatest_2.10" % "2.0.M5b" ,
                             "org.scala-lang" % "scala-reflect" % "2.10.1" )

// Disable parallel execution of the test suite. This is necessary to be able to reference genertaed names in some of the test cases.

parallelExecution in Test := false