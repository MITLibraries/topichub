name := """scoap3hub"""

version := "0.1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.6"

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-language:postfixOps"
)

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  specs2 % Test,
  "org.webjars" % "bootstrap" % "3.3.4",
  "com.amazonaws" % "aws-java-sdk-s3" % "1.9.14",
  "org.scalesxml" %% "scales-xml" % "0.6.0-M3",
  "org.scalesxml" %% "scales-jaxen" % "0.6.0-M3" intransitive(),
  "jaxen" % "jaxen" % "1.1.6" intransitive(),
  "com.typesafe.play" %% "anorm" % "2.4.0-RC1",
  "org.postgresql" % "postgresql" % "9.4-1201-jdbc41",
  "org.mockito" % "mockito-core" % "1.10.19" % "test",
  "org.easytesting" % "fest-assert" % "1.4" % "test"
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

parallelExecution in Test := false
fork in Test := false
