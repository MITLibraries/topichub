name := """scoap3hub"""

version := "0.1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.webjars" %% "webjars-play" % "2.3.0-2",
  "org.webjars" % "bootstrap" % "3.3.2-1",
  "com.amazonaws" % "aws-java-sdk-s3" % "1.9.14",
  "org.scalesxml" %% "scales-xml" % "0.6.0-M3",
  "org.scalesxml" %% "scales-jaxen" % "0.6.0-M3" intransitive(),
  "jaxen" % "jaxen" % "1.1.6" intransitive(),
  "org.postgresql" % "postgresql" % "9.4-1201-jdbc41",
  "org.mockito" % "mockito-core" % "1.10.19" % "test"
)

parallelExecution in Test := false
fork in Test := true
