name := "scala"

version := "0.1"

scalaVersion := "2.13.5"

val projectName = "papan"
val serviceName = "scala"

lazy val root = (project in file("."))
  .settings(
    name := serviceName,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
    )
  )
