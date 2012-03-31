organization := "fr.arnk"

name := "sosmessagedecarte-api"

version := "2.0-SNAPSHOT"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-actor" % "2.0",
  "net.databinder" %% "unfiltered-filter" % "0.6.1",
  "net.databinder" %% "unfiltered-netty-server" % "0.6.1",
  "net.databinder" %% "unfiltered-json" % "0.6.1",
  "com.mongodb.casbah" %% "casbah" % "3.0.0-SNAPSHOT",
  "org.streum" %% "configrity-core" % "0.10.0",
  "javax.mail" % "mail" % "1.4.4",
  "ch.qos.logback" % "logback-classic" % "0.9.28",
  "net.databinder" %% "unfiltered-spec" % "0.6.1" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

seq(scalariformSettings: _*)

parallelExecution in Test := false
