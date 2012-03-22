package fr.arnk.sosmessage

import com.mongodb.casbah.{ MongoCollection, MongoConnection }

object DB {

  lazy val db = {
    val mongo = MongoConnection(SosMessageConfig[String]("database.host").getOrElse("127.0.0.1"),
      SosMessageConfig[Int]("database.port").getOrElse(27017))
    val dataBaseName = SosMessageConfig[String]("database.name").getOrElse("sosmessage")
    mongo(dataBaseName)
  }

  def collection[T](name: String)(f: MongoCollection => T): T = f(db(name))

  def drop(name: String) {
    db(name).drop()
  }

}
