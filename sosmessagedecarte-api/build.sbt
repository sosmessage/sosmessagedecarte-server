organization := "fr.arnk"

name := "sosmessagedecarte-api"

version := "1.1-SNAPSHOT"

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies ++= Seq(
  "net.databinder" %% "unfiltered-filter" % "0.5.3",
  "net.databinder" %% "unfiltered-netty-server" % "0.5.3",
  "net.databinder" %% "unfiltered-json" % "0.5.3",
  "com.mongodb.casbah" %% "casbah" % "3.0.0-SNAPSHOT",
  "org.streum" %% "configrity" % "0.9.0",
  "ch.qos.logback" % "logback-classic" % "0.9.28",
  "net.databinder" %% "unfiltered-spec" % "0.5.0" % "test"
)
