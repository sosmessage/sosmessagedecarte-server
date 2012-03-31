import sbt._
import sbt.Keys._

import PlayProject._
import com.typesafe.sbtscalariform.ScalariformPlugin._

object ApplicationBuild extends Build {

    val appName         = "sosmessagedecarte-admin"
    val appVersion      = "2.0-SNAPSHOT"

    val appDependencies = Seq(
      "org.streum" %% "configrity" % "0.9.0",
      "com.mongodb.casbah" %% "casbah" % "3.0.0-SNAPSHOT",
      "net.liftweb" %% "lift-json" % "2.4-M4",
      "commons-lang" % "commons-lang" % "2.6"
    )

    val buildSettings =
      Seq(
        scalacOptions ++= Seq("-unchecked", "-deprecation")
    )

    val main = PlayProject(appName, appVersion, appDependencies).settings(defaultScalaSettings:_*)
      .settings(buildSettings:_*).settings(scalariformSettings: _*)

}
