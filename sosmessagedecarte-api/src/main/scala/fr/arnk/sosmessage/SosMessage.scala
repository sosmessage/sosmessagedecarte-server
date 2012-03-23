package fr.arnk.sosmessage

import util.Random
import akka.actor.{ Props, ActorSystem }
import com.mongodb.casbah._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import com.mongodb.DBObject
import map_reduce.MapReduceStandardOutput
import java.util.Date
import org.bson.types.ObjectId
import unfiltered.request.Params
import unfiltered.response.NoContent
import MapReduce._

object SosMessageCollections {
  val MessagesCollectionName = "messages"
  val CategoriesCollectionName = "categories"
  val CommentsCollectionName = "comments"
  val AnnouncementsCollectionName = "announcements"
}

case class Category(dbObject: DBObject)
case class Message(dbObject: DBObject)
case class Comment(dbObject: DBObject)
case class Announcement(dbObject: DBObject)

object SosMessage {
  import SosMessageCollections._

  val DefaultSosMessageAppName = "smdc"

  val random = new Random()

  private val emailSender = EmailSender.get

  // Categories
  def categoryExists(categoryId: String): Boolean = {
    DB.collection(CategoriesCollectionName) {
      c =>
        c.findOne(MongoDBObject("_id" -> new ObjectId(categoryId))) match {
          case Some(o) => true
          case None => false
        }
    }
  }

  def publishedCategories(appName: Option[String]): Seq[Category] = {
    val applicationName = appName match {
      case Some(name) => name
      case None => DefaultSosMessageAppName
    }

    val categoryOrder = MongoDBObject("apps." + applicationName + ".order" -> -1)
    val q = MongoDBObject("apps." + applicationName + ".published" -> true)
    DB.collection(CategoriesCollectionName) {
      c =>
        c.find(q).sort(categoryOrder).toSeq.map(category => Category(category))
    }
  }

  // Messages
  def messageExists(messageId: String): Boolean = {
    DB.collection(MessagesCollectionName) {
      c =>
        c.findOne(MongoDBObject("_id" -> new ObjectId(messageId))) match {
          case Some(o) => true
          case None => false
        }
    }
  }

  def randomMessage(categoryId: String, uid: Option[String]): Option[Message] = {
    import MapReduce._
    DB.collection(MessagesCollectionName) {
      c =>
        val q = MongoDBObject("categoryId" -> new ObjectId(categoryId), "state" -> "approved")
        val count = c.find(q, MongoDBObject("_id" -> 1)).count
        val skip = random.nextInt(if (count <= 0) 1 else count)

        val keys = MongoDBObject("_id" -> 1)
        val messages = c.find(q, keys).limit(-1).skip(skip)
        if (!messages.isEmpty) {
          val message = messages.next()

          val jsScope = uid match {
            case Some(u) => Some(MongoDBObject("uid" -> u))
            case None => Some(MongoDBObject("uid" -> ""))
          }

          val q = MongoDBObject("_id" -> message.get("_id"))
          val res = c.mapReduce(mapJS, reduceJS, MapReduceInlineOutput,
            finalizeFunction = Some(finalizeJS), query = Some(q), jsScope = jsScope).next()

          val json = Message(res.get("value").asInstanceOf[DBObject])
          Some(json)
        } else {
          None
        }
    }
  }

  def messages(categoryId: String, uid: Option[String]): Seq[Message] = {
    val jsScope = uid match {
      case Some(u) => Some(MongoDBObject("uid" -> u))
      case None => Some(MongoDBObject("uid" -> ""))
    }

    val q = MongoDBObject("categoryId" -> new ObjectId(categoryId), "state" -> "approved")
    val resultCollectionName = MapReduce.MapReduceMessagesCollectionName + categoryId
    DB.collection(MessagesCollectionName) {
      c =>
        c.mapReduce(mapJS, reduceJS, MapReduceStandardOutput(resultCollectionName),
          finalizeFunction = Some(finalizeJS), query = Some(q), jsScope = jsScope)
    }

    val order = MongoDBObject("value.createdAt" -> -1)
    DB.collection(resultCollectionName) {
      c =>
        c.find().sort(order).toSeq.map(message =>
          Message(message.get("value").asInstanceOf[DBObject]))
    }
  }

  def bestMessages(categoryId: String, uid: Option[String], limit: Option[Int]): Seq[Message] = {
    val jsScope = uid match {
      case Some(u) => Some(MongoDBObject("uid" -> u))
      case None => Some(MongoDBObject("uid" -> ""))
    }

    val q = MongoDBObject("categoryId" -> new ObjectId(categoryId), "state" -> "approved")
    val resultCollectionName = MapReduceMessagesCollectionName + categoryId
    DB.collection(MessagesCollectionName) {
      c =>
        c.mapReduce(mapJS, reduceJS, MapReduceStandardOutput(resultCollectionName),
          finalizeFunction = Some(finalizeJS), query = Some(q), jsScope = jsScope)
    }

    val order = MongoDBObject("value.rating" -> 1)
    DB.collection(resultCollectionName) {
      c =>
        c.find().sort(order).limit(limit.getOrElse(10)).toSeq.map(message =>
          Message(message.get("value").asInstanceOf[DBObject]))
    }
  }

  def worstMessages(categoryId: String, uid: Option[String], limit: Option[Int]): Seq[Message] = {
    val jsScope = uid match {
      case Some(u) => Some(MongoDBObject("uid" -> u))
      case None => Some(MongoDBObject("uid" -> ""))
    }

    val q = MongoDBObject("categoryId" -> new ObjectId(categoryId), "state" -> "approved")
    val resultCollectionName = MapReduceMessagesCollectionName + categoryId
    DB.collection(MessagesCollectionName) {
      c =>
        c.mapReduce(mapJS, reduceJS, MapReduceStandardOutput(resultCollectionName),
          finalizeFunction = Some(finalizeJS), query = Some(q), jsScope = jsScope)
    }

    val order = MongoDBObject("value.rating" -> -1)
    DB.collection(resultCollectionName) {
      c =>
        c.find().sort(order).limit(limit.getOrElse(10)).toSeq.map(message =>
          Message(message.get("value").asInstanceOf[DBObject]))
    }
  }

  def addMessage(categoryId: String, text: String, contributorName: Option[String]) {
    DB.collection(CategoriesCollectionName) {
      c =>
        c.findOne(MongoDBObject("_id" -> new ObjectId(categoryId))) map {
          category =>
            val builder = MongoDBObject.newBuilder
            builder += "categoryId" -> category.get("_id")
            builder += "category" -> category.get("name")
            builder += "text" -> text
            contributorName match {
              case Some(param) =>
                builder += "contributorName" -> param
              case None =>
                builder += "contributorName" -> ""
            }
            builder += "state" -> "waiting"
            builder += "createdAt" -> new Date()
            builder += "modifiedAt" -> new Date()
            builder += "random" -> scala.math.random
            val result = builder.result

            DB.collection(MessagesCollectionName) {
              c =>
                c += result
                emailSender ! SendEmail(result)
            }
        }
    }
  }

  // Rating
  def rateMessage(messageId: String, uid: String, rating: Int): Message = {
    val key = "ratings." + uid.replaceAll("\\.", "-")
    DB.collection(MessagesCollectionName) {
      c =>
        val q = MongoDBObject("_id" -> new ObjectId(messageId))
        c.update(q, $set(key -> rating), false, false)

        val jsScope = Some(MongoDBObject("uid" -> uid))
        val res = c.mapReduce(mapJS, reduceJS, MapReduceInlineOutput,
          finalizeFunction = Some(finalizeJS), query = Some(q), jsScope = jsScope).next()
        Message(res.get("value").asInstanceOf[DBObject])
    }
  }

  // Comments
  def comments(messageId: String, offset: Option[Int] = Some(0), limit: Option[Int] = Some(10)): Seq[Comment] = {
    val q = MongoDBObject("messageId" -> new ObjectId(messageId))
    val order = MongoDBObject("createdAt" -> 1)
    DB.collection(CommentsCollectionName) {
      c =>
        c.find(q).sort(order).skip(offset.getOrElse(0)).limit(limit.getOrElse(10))
          .toSeq.map(comment => Comment(comment))
    }
  }

  def addComment(messageId: String, uid: String, text: String, author: Option[String] = None): Comment = {
    val oid = new ObjectId(messageId)
    val builder = MongoDBObject.newBuilder
    builder += "messageId" -> oid
    builder += "text" -> text
    author.map({
      a =>
        builder += "author" -> a
    })
    builder += "createdAt" -> new Date()
    builder += "uid" -> uid
    val result = builder.result
    DB.collection(CommentsCollectionName) {
      c =>
        c += result
    }
    DB.collection(MessagesCollectionName) {
      c =>
        c.update(MongoDBObject("_id" -> oid), $inc("commentsCount" -> 1), false, false)
    }
    Comment(result)
  }

  // Announcements
  def publishedAnnouncements(appName: Option[String]): Seq[Announcement] = {
    val applicationName = appName match {
      case Some(name) => name
      case None => DefaultSosMessageAppName
    }

    val q = MongoDBObject("apps." + applicationName + ".published" -> true)
    DB.collection(AnnouncementsCollectionName) {
      c =>
        c.find(q).toSeq.map(announcement => Announcement(announcement))
    }
  }

}
