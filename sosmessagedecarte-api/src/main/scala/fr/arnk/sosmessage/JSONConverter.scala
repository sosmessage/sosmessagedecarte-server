package fr.arnk.sosmessage

import com.mongodb.casbah._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import java.util.Date

trait JSONConverter[T] {
  def toJSON(o: T): JValue
}

object StandardConverters {
  implicit object CategoryAsJSON extends JSONConverter[Category] {
    def toJSON(o: Category) = {
      val dbObject = o.dbObject
      ("id", dbObject.get("_id").toString) ~
        ("type", "category") ~
        ("name", dbObject.get("name").toString) ~
        ("color", dbObject.get("color").toString) ~
        ("lastAddedMessageAt", dbObject.get("lastAddedMessageAt").asInstanceOf[Date].getTime)
    }
  }

  implicit object MessageAsJSON extends JSONConverter[Message] {
    def toJSON(o: Message) = {
      val dbObject = o.dbObject
      ("id", dbObject.get("_id").toString) ~
        ("type", "message") ~
        ("category", dbObject.get("category").toString) ~
        ("categoryId", dbObject.get("categoryId").toString) ~
        ("text", dbObject.get("text").toString) ~
        ("createdAt", dbObject.get("createdAt").asInstanceOf[Date].getTime) ~
        ("modifiedAt", dbObject.get("modifiedAt").asInstanceOf[Date].getTime) ~
        ("contributorName", dbObject.get("contributorName").toString) ~
        ("commentsCount", dbObject.get("commentsCount").asInstanceOf[Long]) ~
        ("vote", ("plus", dbObject.get("votePlus").asInstanceOf[Double].toLong) ~
          ("minus", dbObject.get("voteMinus").asInstanceOf[Double].toLong) ~
          ("userVote", dbObject.get("userVote").asInstanceOf[Double].toLong)) ~
          ("rating", ("count", dbObject.get("ratingCount").asInstanceOf[Double].toLong) ~
            ("value", dbObject.get("rating").asInstanceOf[Double]))
    }
  }

  implicit object CommentAsJSON extends JSONConverter[Comment] {
    def toJSON(o: Comment) = {
      val dbObject = o.dbObject
      ("id", dbObject.get("_id").toString) ~
        ("type", "comment") ~
        ("messageId", dbObject.get("messageId").toString) ~
        ("text", dbObject.get("text").toString) ~
        ("author", dbObject.get("author").toString) ~
        ("createdAt", dbObject.get("createdAt").asInstanceOf[Date].getTime) ~
        ("uid", dbObject.get("uid").toString)
    }
  }

  implicit object AnnouncementAsJSON extends JSONConverter[Announcement] {
    def toJSON(o: Announcement) = {
      val dbObject = o.dbObject
      ("id", dbObject.get("_id").toString) ~
        ("type", "announcement") ~
        ("title", dbObject.get("title").toString) ~
        ("text", dbObject.get("text").toString) ~
        ("url", dbObject.get("url").toString) ~
        ("buttons",
          ("validate", dbObject.get("buttons").asInstanceOf[DBObject].get("validate").toString) ~
          ("cancel", dbObject.get("buttons").asInstanceOf[DBObject].get("cancel").toString))
    }
  }

  def toJSON[T](o: T)(implicit converter: JSONConverter[T]) =
    converter.toJSON(o)

  def toJSON[T](xs: Seq[T])(implicit converter: JSONConverter[T]) =
    xs.map(o => converter.toJSON(o))
}
