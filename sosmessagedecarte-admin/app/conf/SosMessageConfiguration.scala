package conf

import org.streum.configrity.Configuration

/**
 * @author <a href="mailto:troger@nuxeo.com">Thomas Roger</a>
 * @since 5.5
 */

object SosMessageConfiguration {

  def getConfig: Configuration = {
    val defaultConfig = Configuration("database.host" -> "127.0.0.1",
      "database.port" -> 27017, "database.name" -> "sosmessage")

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
