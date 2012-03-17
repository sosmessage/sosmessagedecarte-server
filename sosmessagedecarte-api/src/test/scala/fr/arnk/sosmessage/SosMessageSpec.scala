package fr.arnk.sosmessage

import org.specs._

import org.streum.configrity.Configuration
import com.mongodb.casbah._
import net.liftweb.json._
import java.util.Date
import com.mongodb.{ BasicDBObject, DBObject }

object SosMessageSpec extends Specification with unfiltered.spec.netty.Served {

  import dispatch._

  val mockConfig = Configuration("database.host" -> "127.0.0.1",
    "database.port" -> 27017, "database.name" -> "sosmessagedetest", "server.port" -> 3000)

  val MessagesCollectionName = "messages"
  val CategoriesCollectionName = "categories"
  val CommentsCollectionName = "comments"

  val dataBaseName = mockConfig[String]("database.name")

  val mongo = MongoConnection(mockConfig[String]("database.host", "127.0.0.1"), mockConfig[Int]("database.port", 27017))
  val messagesCollection = mongo(dataBaseName)(MessagesCollectionName)
  val categoriesCollection = mongo(dataBaseName)(CategoriesCollectionName)
  val commentsCollection = mongo(dataBaseName)(CommentsCollectionName)

  def setup = {
    _.handler(new SosMessage(mockConfig))
  }

  "The SosMessage app" should {
    doBefore {
      initializeDB()
    }

    "retrieve ordered published categories" in {
      val resp = http(host / "api" / "v1" / "categories" as_str)
      val json = parse(resp)

      json \ "count" must_== JInt(3)

      val JArray(items) = json \ "items"
      items.size must_== 3

      val firstItem = items(0)
      firstItem \ "name" must_== JString("firstCategory")
      firstItem \ "color" must_== JString("#000")

      val secondItem = items(1)
      secondItem \ "name" must_== JString("secondCategory")
      secondItem \ "color" must_== JString("#fff")

      val thirdItem = items(2)
      thirdItem \ "name" must_== JString("fourthCategory")
      thirdItem \ "color" must_== JString("#00f")
    }

    "retrieve ordered published categories for the smdt appname" in {
      val resp = http(host / "api" / "v1" / "categories" <<? Map("appname" -> "smdt") as_str)
      val json = parse(resp)

      json \ "count" must_== JInt(2)

      val JArray(items) = json \ "items"
      items.size must_== 2

      val firstItem = items(0)
      firstItem \ "name" must_== JString("fifthCategory")
      firstItem \ "color" must_== JString("#0ff")

      val secondItem = items(1)
      secondItem \ "name" must_== JString("fourthCategory")
      secondItem \ "color" must_== JString("#00f")

    }

    "retrieve only approved messages in firstCategory" in {
      val firstCategory = categoriesCollection.findOne(MongoDBObject("name" -> "firstCategory")).get
      val resp = http(host / "api" / "v1" / "categories" / firstCategory.get("_id").toString / "messages" as_str)
      val json = parse(resp)

      json \ "count" must_== JInt(2)

      val JArray(items) = json \ "items"
      items.size must_== 2

      val firstItem = items(0)
      firstItem \ "text" must_== JString("Third message in first category")

      val secondItem = items(1)
      secondItem \ "text" must_== JString("First message in first category")
    }

    "retrieve only approved messages in secondCategory" in {
      val secondCategory = categoriesCollection.findOne(MongoDBObject("name" -> "secondCategory")).get
      val resp = http(host / "api" / "v1" / "categories" / secondCategory.get("_id").toString / "messages" as_str)
      val json = parse(resp)

      json \ "count" must_== JInt(2)

      val JArray(items) = json \ "items"
      items.size must_== 2

      val firstItem = items(0)
      firstItem \ "text" must_== JString("First message in second category")

      val secondItem = items(1)
      secondItem \ "text" must_== JString("Second message in second category")
    }

    "retrieve one message among the approved ones" in {
      val firstCategory = categoriesCollection.findOne(MongoDBObject("name" -> "firstCategory")).get

      val expectedFirstCategoryMessages = List(JString("First message in first category"), JString("Third message in first category"))
      (1 until 50).map {
        index =>
          val resp = http(host / "api" / "v1" / "categories" / firstCategory.get("_id").toString / "message" as_str)
          val json = parse(resp)
          expectedFirstCategoryMessages mustContain (json \ "text").asInstanceOf[JString]
      }

      val secondCategory = categoriesCollection.findOne(MongoDBObject("name" -> "secondCategory")).get
      val expectedSecondCategoryMessages = List(JString("First message in second category"), JString("Second message in second category"))
      (1 until 50).map {
        index =>
          val resp = http(host / "api" / "v1" / "categories" / secondCategory.get("_id").toString / "message" as_str)
          val json = parse(resp)
          expectedSecondCategoryMessages mustContain (json \ "text").asInstanceOf[JString]
      }
    }

    "create a message in the given category" in {
      val fourthCategory = categoriesCollection.findOne(MongoDBObject("name" -> "fourthCategory")).get
      http(host / "api" / "v1" / "categories" / fourthCategory.get("_id").toString / "message"
        << Map("text" -> "test message") >|)

      val messageOrder = MongoDBObject("createdAt" -> -1)
      val q = MongoDBObject("categoryId" -> fourthCategory.get("_id"))
      val keys = MongoDBObject("category" -> 1, "categoryId" -> 1, "text" -> 1, "createdAt" -> 1)
      val messages = messagesCollection.find(q, keys).sort(messageOrder).foldLeft(List[DBObject]())((l, a) =>
        a :: l
      ).reverse

      messages.size must_== 2
      val message = messages(1)
      message.get("text").toString must_== "test message"
      message.get("categoryId").toString must_== fourthCategory.get("_id").toString
      message.get("category").toString must_== fourthCategory.get("name").toString
    }

    "create a message in the given category with a contributor name" in {
      val fourthCategory = categoriesCollection.findOne(MongoDBObject("name" -> "fourthCategory")).get
      http(host / "api" / "v1" / "categories" / fourthCategory.get("_id").toString / "message"
        << Map("text" -> "bender message", "contributorName" -> "Bender") >|)

      val messageOrder = MongoDBObject("createdAt" -> -1)
      val q = MongoDBObject("categoryId" -> fourthCategory.get("_id"))
      val keys = MongoDBObject("category" -> 1, "categoryId" -> 1, "text" -> 1, "contributorName" -> 1, "createdAt" -> 1)
      val messages = messagesCollection.find(q, keys).sort(messageOrder).foldLeft(List[DBObject]())((l, a) =>
        a :: l
      ).reverse

      messages.size must_== 2
      val message = messages(1)
      message.get("text").toString must_== "bender message"
      message.get("contributorName").toString must_== "Bender"
      message.get("categoryId").toString must_== fourthCategory.get("_id").toString
      message.get("category").toString must_== fourthCategory.get("name").toString
    }

    "rate a given message" in {
      val q = MongoDBObject("text" -> "Second message in second category")
      var message = messagesCollection.findOne(q).get

      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone1", "rating" -> "4") >|)
      message = messagesCollection.findOne(q).get
      var ratings = message.get("ratings").asInstanceOf[BasicDBObject]
      ratings.containsField("iphone1") mustBe true
      ratings.getLong("iphone1") must_== 4

      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone1", "rating" -> "2") >|)
      message = messagesCollection.findOne(q).get
      ratings = message.get("ratings").asInstanceOf[BasicDBObject]
      ratings.containsField("iphone1") mustBe true
      ratings.getLong("iphone1") must_== 2

      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "android1", "rating" -> "3") >|)
      message = messagesCollection.findOne(q).get
      ratings = message.get("ratings").asInstanceOf[BasicDBObject]
      ratings.containsField("iphone1") mustBe true
      ratings.getLong("iphone1") must_== 2
      ratings.containsField("android1") mustBe true
      ratings.getLong("android1") must_== 3
    }

    "retrieve rating with message" in {
      val message = messagesCollection.findOne(MongoDBObject("text" -> "First message in third category")).get
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone1", "rating" -> "3") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone2", "rating" -> "4") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone3", "rating" -> "1") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone4", "rating" -> "3") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "android1", "rating" -> "2") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "android2", "rating" -> "4") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "android3", "rating" -> "3") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "android4", "rating" -> "4") >|)

      val thirdCategory = categoriesCollection.findOne(MongoDBObject("name" -> "thirdCategory")).get
      val resp = http(host / "api" / "v1" / "categories" / thirdCategory.get("_id").toString / "message" as_str)
      val json = parse(resp)

      json \ "text" must_== JString("First message in third category")
      json \ "rating" \ "value" must_== JDouble(3.0)
      json \ "rating" \ "count" must_== JInt(8)
    }

    "retrieve votes with message" in {
      val message = messagesCollection.findOne(MongoDBObject("text" -> "First message in fourth category")).get
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "iphone1", "vote" -> "1") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "iphone2", "vote" -> "-1") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "iphone3", "vote" -> "-1") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "iphone4", "vote" -> "-1") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "android1", "vote" -> "-1") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "android2", "vote" -> "1") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "android3", "vote" -> "-1") >|)
      http(host / "api" / "v1" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "android4", "vote" -> "1") >|)

      val fourthCategory = categoriesCollection.findOne(MongoDBObject("name" -> "fourthCategory")).get
      var resp = http(host / "api" / "v1" / "categories" / fourthCategory.get("_id").toString / "message" as_str)
      var json = parse(resp)

      json \ "text" must_== JString("First message in fourth category")
      json \ "vote" \ "plus" must_== JInt(3)
      json \ "vote" \ "minus" must_== JInt(5)
      json \ "vote" \ "userVote" must_== JInt(0)

      resp = http(host / "api" / "v1" / "categories" / fourthCategory.get("_id").toString / "message" <<? Map("uid" -> "iphone1") as_str)
      json = parse(resp)

      json \ "text" must_== JString("First message in fourth category")
      json \ "vote" \ "plus" must_== JInt(3)
      json \ "vote" \ "minus" must_== JInt(5)
      json \ "vote" \ "userVote" must_== JInt(1)

      resp = http(host / "api" / "v1" / "categories" / fourthCategory.get("_id").toString / "message" <<? Map("uid" -> "android1") as_str)
      json = parse(resp)

      json \ "text" must_== JString("First message in fourth category")
      json \ "vote" \ "plus" must_== JInt(3)
      json \ "vote" \ "minus" must_== JInt(5)
      json \ "vote" \ "userVote" must_== JInt(-1)
    }

    "create comments for the given message" in {
      val aMessage = messagesCollection.findOne(MongoDBObject("text" -> "First message in first category")).get
      http(host / "api" / "v1" / "messages" / aMessage.get("_id").toString / "comments"
        << Map("text" -> "Bender's comment", "author" -> "Bender", "uid" -> "android1") >|)
      http(host / "api" / "v1" / "messages" / aMessage.get("_id").toString / "comments"
        << Map("text" -> "Leela's comment", "author" -> "Leela", "uid" -> "iphone1") >|)

      val updatedMessage = messagesCollection.findOne(MongoDBObject("text" -> "First message in first category")).get
      updatedMessage.asInstanceOf[BasicDBObject].getLong("commentsCount") must_== 2

      val resp = http(host / "api" / "v1" / "messages" / updatedMessage.get("_id").toString / "comments" as_str)
      val json = parse(resp)

      json \ "count" must_== JInt(2)

      val JArray(items) = json \ "items"
      items.size must_== 2

      val firstItem = items(0)
      firstItem \ "text" must_== JString("Bender's comment")
      firstItem \ "author" must_== JString("Bender")
      firstItem \ "messageId" must_== JString(updatedMessage.get("_id").toString)
      firstItem \ "uid" must_== JString("android1")

      val secondItem = items(1)
      secondItem \ "text" must_== JString("Leela's comment")
      secondItem \ "author" must_== JString("Leela")
      secondItem \ "messageId" must_== JString(updatedMessage.get("_id").toString)
      secondItem \ "uid" must_== JString("iphone1")
    }
  }

  def initializeDB() {
    messagesCollection.drop()
    categoriesCollection.drop()

    createCategories()
    createMessages()
  }

  def createCategories() {
    val smdcAppKey = "apps.smdc"
    val smdtAppKey = "apps.smdt"

    val date = new Date()
    var builder = MongoDBObject.newBuilder
    builder += "name" -> "firstCategory"
    builder += "color" -> "#000"
    builder += "createdAt" -> date
    builder += "modifiedAt" -> date
    builder += "lastAddedMessageAt" -> date
    categoriesCollection += builder.result
    categoriesCollection.update(MongoDBObject("name" -> "firstCategory"), $set(smdcAppKey ->
      MongoDBObject("published" -> true, "order" -> 3), "modifiedAt" -> new Date()), false, false)

    builder = MongoDBObject.newBuilder
    builder += "name" -> "secondCategory"
    builder += "color" -> "#fff"
    builder += "createdAt" -> new Date(date.getTime + 10000)
    builder += "modifiedAt" -> new Date(date.getTime + 10000)
    builder += "lastAddedMessageAt" -> date
    categoriesCollection += builder.result
    categoriesCollection.update(MongoDBObject("name" -> "secondCategory"), $set(smdcAppKey ->
      MongoDBObject("published" -> true, "order" -> 2), "modifiedAt" -> new Date()), false, false)

    builder = MongoDBObject.newBuilder
    builder += "name" -> "thirdCategory"
    builder += "color" -> "#0f0"
    builder += "createdAt" -> new Date(date.getTime + 20000)
    builder += "modifiedAt" -> new Date(date.getTime + 20000)
    builder += "lastAddedMessageAt" -> date
    categoriesCollection += builder.result
    categoriesCollection.update(MongoDBObject("name" -> "thirdCategory"), $set(smdcAppKey ->
      MongoDBObject("published" -> false, "order" -> 1), "modifiedAt" -> new Date()), false, false)

    builder = MongoDBObject.newBuilder
    builder += "name" -> "fourthCategory"
    builder += "color" -> "#00f"
    builder += "createdAt" -> new Date()
    builder += "modifiedAt" -> new Date()
    builder += "lastAddedMessageAt" -> date
    categoriesCollection += builder.result
    categoriesCollection.update(MongoDBObject("name" -> "fourthCategory"), $set(smdcAppKey ->
      MongoDBObject("published" -> true, "order" -> 0), "modifiedAt" -> new Date()), false, false)
    categoriesCollection.update(MongoDBObject("name" -> "fourthCategory"), $set(smdtAppKey ->
      MongoDBObject("published" -> true, "order" -> 0), "modifiedAt" -> new Date()), false, false)

    builder = MongoDBObject.newBuilder
    builder += "name" -> "fifthCategory"
    builder += "color" -> "#0ff"
    builder += "createdAt" -> new Date()
    builder += "modifiedAt" -> new Date()
    builder += "lastAddedMessageAt" -> date
    categoriesCollection += builder.result
    categoriesCollection.update(MongoDBObject("name" -> "fifthCategory"), $set(smdtAppKey ->
      MongoDBObject("published" -> true, "order" -> 1), "modifiedAt" -> new Date()), false, false)
  }

  def createMessages() {
    val firstCategory = categoriesCollection.findOne(MongoDBObject("name" -> "firstCategory")).get
    val secondCategory = categoriesCollection.findOne(MongoDBObject("name" -> "secondCategory")).get
    val thirdCategory = categoriesCollection.findOne(MongoDBObject("name" -> "thirdCategory")).get
    val fourthCategory = categoriesCollection.findOne(MongoDBObject("name" -> "fourthCategory")).get

    val date = new Date()
    var builder = MongoDBObject.newBuilder
    builder += "categoryId" -> firstCategory.get("_id")
    builder += "category" -> firstCategory.get("name")
    builder += "text" -> "First message in first category"
    builder += "contributorName" -> ""
    builder += "commentsCount" -> 0L
    builder += "state" -> "approved"
    builder += "state" -> "approved"
    builder += "createdAt" -> new Date(date.getTime + 10000)
    builder += "modifiedAt" -> new Date(date.getTime + 10000)
    builder += "random" -> scala.math.random
    messagesCollection += builder.result

    builder = MongoDBObject.newBuilder
    builder += "categoryId" -> firstCategory.get("_id")
    builder += "category" -> firstCategory.get("name")
    builder += "text" -> "Second message in first category"
    builder += "contributorName" -> ""
    builder += "commentsCount" -> 0L
    builder += "state" -> "waiting"
    builder += "createdAt" -> new Date(date.getTime + 15000)
    builder += "modifiedAt" -> new Date(date.getTime + 15000)
    builder += "random" -> scala.math.random
    messagesCollection += builder.result

    builder = MongoDBObject.newBuilder
    builder += "categoryId" -> firstCategory.get("_id")
    builder += "category" -> firstCategory.get("name")
    builder += "text" -> "Third message in first category"
    builder += "contributorName" -> ""
    builder += "commentsCount" -> 0L
    builder += "state" -> "approved"
    builder += "createdAt" -> new Date(date.getTime + 20000)
    builder += "modifiedAt" -> new Date(date.getTime + 20000)
    builder += "random" -> scala.math.random
    messagesCollection += builder.result

    builder = MongoDBObject.newBuilder
    builder += "categoryId" -> secondCategory.get("_id")
    builder += "category" -> secondCategory.get("name")
    builder += "text" -> "First message in second category"
    builder += "contributorName" -> ""
    builder += "commentsCount" -> 0L
    builder += "state" -> "approved"
    builder += "createdAt" -> new Date(date.getTime + 20000)
    builder += "modifiedAt" -> new Date(date.getTime + 20000)
    builder += "random" -> scala.math.random
    messagesCollection += builder.result

    builder = MongoDBObject.newBuilder
    builder += "categoryId" -> secondCategory.get("_id")
    builder += "category" -> secondCategory.get("name")
    builder += "text" -> "Second message in second category"
    builder += "contributorName" -> ""
    builder += "commentsCount" -> 0L
    builder += "state" -> "approved"
    builder += "createdAt" -> new Date(date.getTime + 20000)
    builder += "modifiedAt" -> new Date(date.getTime + 20000)
    builder += "random" -> scala.math.random
    messagesCollection += builder.result

    builder = MongoDBObject.newBuilder
    builder += "categoryId" -> thirdCategory.get("_id")
    builder += "category" -> thirdCategory.get("name")
    builder += "text" -> "First message in third category"
    builder += "contributorName" -> ""
    builder += "commentsCount" -> 0L
    builder += "state" -> "approved"
    builder += "createdAt" -> new Date(date.getTime + 20000)
    builder += "modifiedAt" -> new Date(date.getTime + 20000)
    builder += "random" -> scala.math.random
    messagesCollection += builder.result

    builder = MongoDBObject.newBuilder
    builder += "categoryId" -> fourthCategory.get("_id")
    builder += "category" -> fourthCategory.get("name")
    builder += "text" -> "First message in fourth category"
    builder += "contributorName" -> ""
    builder += "commentsCount" -> 0L
    builder += "state" -> "approved"
    builder += "createdAt" -> new Date(date.getTime + 30000)
    builder += "modifiedAt" -> new Date(date.getTime + 30000)
    builder += "random" -> scala.math.random
    messagesCollection += builder.result
  }

}
