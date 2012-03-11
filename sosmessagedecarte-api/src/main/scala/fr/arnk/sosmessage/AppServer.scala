package fr.arnk.sosmessage

import org.streum.configrity.Configuration

object AppServer {

  def main(args: Array[String]) {
    val config = getConfig
    unfiltered.netty.Http(config[Int]("server.port", 3000)).handler(new SosMessage(config)).run
  }

  def getConfig: Configuration = {
    val defaultConfig = Configuration("database.host" -> "127.0.0.1",
      "database.port" -> 27017, "database.name" -> "sosmessage", "server.port" -> 3000)

    val systemConfig = Configuration.systemProperties
    systemConfig.get[String]("sosmessage.configurationFile") match {
      case None => defaultConfig
      case Some(filename) =>
        try {
          Configuration.load(filename)
        } catch {
          case e: Exception => defaultConfig
        }
    }
  }

}
