import sbt._

import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "sosmessagedecarte-admin"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "com.mongodb.casbah" %% "casbah" % "3.0.0-SNAPSHOT",
      "net.liftweb" %% "lift-json" % "2.4-M4",
      "commons-lang" % "commons-lang" % "2.6"
    )

    val main = PlayProject(appName, appVersion, appDependencies).settings(defaultScalaSettings:_*).settings(
      // Add your own project settings here
    )

}
