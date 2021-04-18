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
      "org.scalatestplus" %% "scalatestplus-mockito" % "1.0.0-M2" % Test,
      "com.typesafe" % "config" % "1.4.1",
      "com.iheart" %% "ficus" % "1.5.0",
      "com.typesafe.play" %% "play-json" % "2.10.0-RC2",
    )
  )
