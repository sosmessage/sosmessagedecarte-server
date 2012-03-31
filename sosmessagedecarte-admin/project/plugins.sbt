// Comment to get more information during initialization
logLevel := Level.Warn

resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("com.typesafe.sbtscalariform" % "sbtscalariform" % "0.3.0")

addSbtPlugin("play" % "sbt-plugin" % "2.0")
