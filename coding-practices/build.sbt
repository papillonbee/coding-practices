name := "coding-practices"

version := "0.1"

scalaVersion := "2.13.5"

val projectName = "papan"
val serviceName = "coding-practices"

lazy val root = (project in file("."))
  .settings(
    name := serviceName,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
    )
  )
