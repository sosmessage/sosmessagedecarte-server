package fr.arnk.sosmessage.v1

import fr.arnk.sosmessage._

import org.specs._
import unfiltered._
import org.streum.configrity.Configuration
import com.mongodb.casbah._
import net.liftweb.json._
import java.util.Date
import com.mongodb.{ BasicDBObject, DBObject }

object CommentsSpec extends SosMessageSpec {

  import SosMessageCollections._

  "The comments API v1" should {
    doBefore {
      TestDB.initialize
    }

    "create comments for the given message" in {
      DB.collection(MessagesCollectionName) {
        c =>
          val aMessage = c.findOne(MongoDBObject("text" -> "First message in first category")).get
          http(host / "api" / "v1" / "messages" / aMessage.get("_id").toString / "comments"
            << Map("text" -> "Bender's comment", "author" -> "Bender", "uid" -> "android1") >|)
          http(host / "api" / "v1" / "messages" / aMessage.get("_id").toString / "comments"
            << Map("text" -> "Leela's comment", "author" -> "Leela", "uid" -> "iphone1") >|)

          val updatedMessage = c.findOne(MongoDBObject("text" -> "First message in first category")).get
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
  }

}