package fr.arnk.sosmessage

import org.specs._
import unfiltered._
import org.streum.configrity.Configuration
import com.mongodb.casbah._
import net.liftweb.json._
import java.util.Date
import com.mongodb.{ BasicDBObject, DBObject }
import akka.actor.ActorSystem

object MessagesSpec extends SosMessageSpec {

  import SosMessageCollections._

  "The messages API v2" should {
    doBefore {
      TestDB.initialize
      // stop the Actor sending email
      EmailSender.stop
    }

    "retrieve only approved messages in firstCategory" in {
      val firstCategory = DB.collection(CategoriesCollectionName) {
        c =>
          c.findOne(MongoDBObject("name" -> "firstCategory")).get
      }
      val resp = http(host / "api" / "v2" / "categories" / firstCategory.get("_id").toString / "messages" as_str)
      val json = parse(resp)

      json \ "meta" \ "code" must_== JInt(200)
      val response = json \ "response"
      response \ "count" must_== JInt(2)

      val JArray(items) = response \ "items"
      items.size must_== 2

      val firstItem = items(0)
      firstItem \ "text" must_== JString("Third message in first category")

      val secondItem = items(1)
      secondItem \ "text" must_== JString("First message in first category")
    }

    "retrieve only approved messages in secondCategory" in {
      val secondCategory = DB.collection(CategoriesCollectionName) {
        c =>
          c.findOne(MongoDBObject("name" -> "secondCategory")).get
      }
      val resp = http(host / "api" / "v2" / "categories" / secondCategory.get("_id").toString / "messages" as_str)
      val json = parse(resp)

      json \ "meta" \ "code" must_== JInt(200)
      val response = json \ "response"
      response \ "count" must_== JInt(2)

      val JArray(items) = response \ "items"
      items.size must_== 2

      val firstItem = items(0)
      firstItem \ "text" must_== JString("First message in second category")

      val secondItem = items(1)
      secondItem \ "text" must_== JString("Second message in second category")
    }

    "retrieve one message among the approved ones" in {
      val firstCategory = DB.collection(CategoriesCollectionName) {
        c =>
          c.findOne(MongoDBObject("name" -> "firstCategory")).get
      }
      val expectedFirstCategoryMessages = List(JString("First message in first category"), JString("Third message in first category"))
      (1 until 50).map {
        index =>
          val resp = http(host / "api" / "v2" / "categories" / firstCategory.get("_id").toString / "message" as_str)
          val json = parse(resp)
          json \ "meta" \ "code" must_== JInt(200)
          val response = json \ "response"
          expectedFirstCategoryMessages mustContain (response \ "text").asInstanceOf[JString]
      }

      val secondCategory = DB.collection(CategoriesCollectionName) {
        c =>
          c.findOne(MongoDBObject("name" -> "secondCategory")).get
      }
      val expectedSecondCategoryMessages = List(JString("First message in second category"), JString("Second message in second category"))
      (1 until 50).map {
        index =>
          val resp = http(host / "api" / "v2" / "categories" / secondCategory.get("_id").toString / "message" as_str)
          val json = parse(resp)
          json \ "meta" \ "code" must_== JInt(200)
          val response = json \ "response"
          expectedSecondCategoryMessages mustContain (response \ "text").asInstanceOf[JString]
      }
    }

    "create a message in the given category" in {
      val fourthCategory = DB.collection(CategoriesCollectionName) {
        c =>
          c.findOne(MongoDBObject("name" -> "fourthCategory")).get
      }
      val resp = http(host / "api" / "v2" / "categories" / fourthCategory.get("_id").toString / "message"
        << Map("text" -> "test message") as_str)
      val json = parse(resp)
      json \ "meta" \ "code" must_== JInt(200)

      val messageOrder = MongoDBObject("createdAt" -> -1)
      val q = MongoDBObject("categoryId" -> fourthCategory.get("_id"))
      val keys = MongoDBObject("category" -> 1, "categoryId" -> 1, "text" -> 1, "createdAt" -> 1)

      val messages = DB.collection(MessagesCollectionName) {
        c =>
          c.find(q, keys).sort(messageOrder).foldLeft(List[DBObject]())((l, a) =>
            a :: l
          ).reverse
      }
      messages.size must_== 2
      val message = messages(1)
      message.get("text").toString must_== "test message"
      message.get("categoryId").toString must_== fourthCategory.get("_id").toString
      message.get("category").toString must_== fourthCategory.get("name").toString
    }

    "create a message in the given category with a contributor name" in {
      val fourthCategory = DB.collection(CategoriesCollectionName) {
        c =>
          c.findOne(MongoDBObject("name" -> "fourthCategory")).get
      }
      val resp = http(host / "api" / "v2" / "categories" / fourthCategory.get("_id").toString / "message"
        << Map("text" -> "bender message", "contributorName" -> "Bender") as_str)
      val json = parse(resp)
      json \ "meta" \ "code" must_== JInt(200)

      val messageOrder = MongoDBObject("createdAt" -> -1)
      val q = MongoDBObject("categoryId" -> fourthCategory.get("_id"))
      val keys = MongoDBObject("category" -> 1, "categoryId" -> 1, "text" -> 1, "contributorName" -> 1, "createdAt" -> 1)
      val messages = DB.collection(MessagesCollectionName) {
        c =>
          c.find(q, keys).sort(messageOrder).foldLeft(List[DBObject]())((l, a) =>
            a :: l
          ).reverse
      }
      messages.size must_== 2
      val message = messages(1)
      message.get("text").toString must_== "bender message"
      message.get("contributorName").toString must_== "Bender"
      message.get("categoryId").toString must_== fourthCategory.get("_id").toString
      message.get("category").toString must_== fourthCategory.get("name").toString
    }

    "rate a given message" in {
      DB.collection(MessagesCollectionName) {
        c =>
          val q = MongoDBObject("text" -> "Second message in second category")
          var message = c.findOne(q).get

          var resp = http(host / "api" / "v2" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone1", "rating" -> "4") as_str)
          var json = parse(resp)
          json \ "meta" \ "code" must_== JInt(200)

          message = c.findOne(q).get
          var ratings = message.get("ratings").asInstanceOf[BasicDBObject]
          ratings.containsField("iphone1") mustBe true
          ratings.getLong("iphone1") must_== 4

          resp = http(host / "api" / "v2" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone1", "rating" -> "2") as_str)
          json = parse(resp)
          json \ "meta" \ "code" must_== JInt(200)

          message = c.findOne(q).get
          ratings = message.get("ratings").asInstanceOf[BasicDBObject]
          ratings.containsField("iphone1") mustBe true
          ratings.getLong("iphone1") must_== 2

          resp = http(host / "api" / "v2" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "android1", "rating" -> "3") as_str)
          json = parse(resp)
          json \ "meta" \ "code" must_== JInt(200)

          message = c.findOne(q).get
          ratings = message.get("ratings").asInstanceOf[BasicDBObject]
          ratings.containsField("iphone1") mustBe true
          ratings.getLong("iphone1") must_== 2
          ratings.containsField("android1") mustBe true
          ratings.getLong("android1") must_== 3
      }
    }

    "retrieve rating with message" in {
      val message = DB.collection(MessagesCollectionName) {
        c =>
          c.findOne(MongoDBObject("text" -> "First message in third category")).get
      }
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone1", "rating" -> "3") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone2", "rating" -> "4") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone3", "rating" -> "1") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "iphone4", "rating" -> "3") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "android1", "rating" -> "2") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "android2", "rating" -> "4") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "android3", "rating" -> "3") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "rate" << Map("uid" -> "android4", "rating" -> "4") >|)

      val thirdCategory = DB.collection(CategoriesCollectionName) {
        c =>
          c.findOne(MongoDBObject("name" -> "thirdCategory")).get
      }
      val resp = http(host / "api" / "v2" / "categories" / thirdCategory.get("_id").toString / "message" as_str)
      val json = parse(resp)
      val response = json \ "response"

      response \ "text" must_== JString("First message in third category")
      response \ "rating" \ "value" must_== JDouble(3.0)
      response \ "rating" \ "count" must_== JInt(8)
    }

    "retrieve votes with message" in {
      val message = DB.collection(MessagesCollectionName) {
        c =>
          c.findOne(MongoDBObject("text" -> "First message in fourth category")).get
      }
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "iphone1", "vote" -> "1") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "iphone2", "vote" -> "-1") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "iphone3", "vote" -> "-1") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "iphone4", "vote" -> "-1") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "android1", "vote" -> "-1") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "android2", "vote" -> "1") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "android3", "vote" -> "-1") >|)
      http(host / "api" / "v2" / "messages" / message.get("_id").toString / "vote" << Map("uid" -> "android4", "vote" -> "1") >|)

      val fourthCategory = DB.collection(CategoriesCollectionName) {
        c =>
          c.findOne(MongoDBObject("name" -> "fourthCategory")).get
      }
      var resp = http(host / "api" / "v2" / "categories" / fourthCategory.get("_id").toString / "message" as_str)
      var json = parse(resp)
      json \ "meta" \ "code" must_== JInt(200)
      var response = json \ "response"

      response \ "text" must_== JString("First message in fourth category")
      response \ "vote" \ "plus" must_== JInt(3)
      response \ "vote" \ "minus" must_== JInt(5)
      response \ "vote" \ "userVote" must_== JInt(0)

      resp = http(host / "api" / "v2" / "categories" / fourthCategory.get("_id").toString / "message" <<? Map("uid" -> "iphone1") as_str)
      json = parse(resp)
      json \ "meta" \ "code" must_== JInt(200)
      response = json \ "response"

      response \ "text" must_== JString("First message in fourth category")
      response \ "vote" \ "plus" must_== JInt(3)
      response \ "vote" \ "minus" must_== JInt(5)
      response \ "vote" \ "userVote" must_== JInt(1)

      resp = http(host / "api" / "v2" / "categories" / fourthCategory.get("_id").toString / "message" <<? Map("uid" -> "android1") as_str)
      json = parse(resp)
      json \ "meta" \ "code" must_== JInt(200)
      response = json \ "response"

      response \ "text" must_== JString("First message in fourth category")
      response \ "vote" \ "plus" must_== JInt(3)
      response \ "vote" \ "minus" must_== JInt(5)
      response \ "vote" \ "userVote" must_== JInt(-1)
    }
  }

}
