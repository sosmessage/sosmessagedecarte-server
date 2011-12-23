package fr.arnk.sosmessage

import org.specs._

import org.streum.configrity.Configuration
import com.mongodb.casbah.MongoConnection
import com.mongodb.casbah.commons.MongoDBObject
import net.liftweb.json._
import java.util.Date

object SosMessageSpec extends Specification with unfiltered.spec.netty.Served {

  import dispatch._

  val mockConfig = Configuration("database.host" -> "127.0.0.1",
        "database.port" -> 27017, "database.name" -> "sosmessagedetest", "server.port" -> 3000)

  val MessagesCollectionName = "messages"
  val CategoriesCollectionName = "categories"

  val dataBaseName = mockConfig[String]("database.name")

  val mongo = MongoConnection(mockConfig[String]("database.host", "127.0.0.1"), mockConfig[Int]("database.port", 27017))
  val messagesCollection = mongo(dataBaseName)(MessagesCollectionName)
  val categoriesCollection = mongo(dataBaseName)(CategoriesCollectionName)

  def setup = { _.handler(new SosMessage(mockConfig)) }

  val http = new Http

  "The example app" should {
    doBefore {
      messagesCollection.drop()
      categoriesCollection.drop()

      val builder = MongoDBObject.newBuilder
      builder += "name" -> "firstCategory"
      builder += "createdAt" -> new Date()
      builder += "modifiedAt" -> new Date()
      categoriesCollection += builder.result
    }
    "serve unfiltered text" in {
      val resp = http(host / "api" / "v1" / "categories" as_str)
      val json = parse(resp)

      json \ "count" must_== JInt(1)

      val JArray(items) = json \ "items"
      items.size must_== 1

      val firstItem = items(0)
      firstItem \ "name" must_== JString("firstCategory")
    }
  }

}
