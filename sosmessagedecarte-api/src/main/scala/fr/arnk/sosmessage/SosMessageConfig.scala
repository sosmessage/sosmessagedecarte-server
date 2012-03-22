package fr.arnk.sosmessage

import org.streum.configrity.Configuration
import org.streum.configrity.converter.ValueConverter
import org.streum.configrity._

object SosMessageConfig {

  private lazy val initialConfig: Configuration = {
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

  private var config: Configuration = initialConfig

  def apply[T](key: String)(implicit converter: ValueConverter[T]) = {
    get[T](key)
  }

  def get[T](key: String)(implicit converter: ValueConverter[T]) = {
    config.get[T](key)
  }

  def set[T](key: String, value: T) = {
    config = config.set(key, value)
  }

}
