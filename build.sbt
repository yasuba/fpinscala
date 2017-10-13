name := "fpinscala"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

scalacOptions := Seq(
  "-unchecked", "-deprecation", "-feature", "-language:postfixOps", "-encoding", "utf8",
  "-Ywarn-adapted-args", "-Ywarn-dead-code", "-Ywarn-inaccessible"
)