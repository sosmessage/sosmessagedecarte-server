package fr.arnk.sosmessage

import scala.util.Random

import unfiltered.request._
import unfiltered.response._
import unfiltered.netty._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Printer._
import com.mongodb.casbah.commons.MongoDBObject
import org.bson.types.ObjectId
import com.mongodb.casbah._
import java.util.Date
import map_reduce.MapReduceStandardOutput
import org.streum.configrity.Configuration

class SosMessage(config: Configuration) extends async.Plan with ServerErrorResponse {

  val MessagesCollectionName = "messages"
  val CategoriesCollectionName = "categories"
  val MapReduceMessagesCollectionName = "mapReduceMessages_"

  val dataBaseName = config[String]("database.name", "sosmessage")

  val mongo = MongoConnection(config[String]("database.host", "127.0.0.1"), config[Int]("database.port", 27017))
  val messagesCollection = mongo(dataBaseName)(MessagesCollectionName)
  val categoriesCollection = mongo(dataBaseName)(CategoriesCollectionName)

  val random = new Random()

  val mapJS = """
    function() {
      emit(this._id, this);
    }
  """

  val reduceJS = """
    function(key, values) {
    }
  """

  val finalizeJS = """
    function(key, value) {
      var count = 0;
      var total = 0;
      for (var prop in value.ratings) {
        count++;
        total += value.ratings[prop];
      }

      if (total == 0 || count == 0) {
        avg = 0;
      } else {
        avg = total / count;
      }

      value.ratingCount = count;
      value.rating = avg;
      delete value.ratings;
      return value;
    }
  """

  def intent = {
    case req @ GET(Path("/api/v1/categories")) =>
      val categoryOrder = MongoDBObject("order" -> -1)
      val q = MongoDBObject("published" -> true)
      val categories = categoriesCollection.find(q).sort(categoryOrder).foldLeft(List[JValue]())((l, a) =>
        categoryToJSON(a) :: l
      ).reverse
      val json = ("count", categories.size) ~ ("items", categories)
      req.respond(JsonContent ~> ResponseString(pretty(render(json))))

    case req @ GET(Path(Seg("api" :: "v1" :: "categories" :: id :: "messages" :: Nil))) =>
      val q = MongoDBObject("categoryId" -> new ObjectId(id), "state" -> "approved")
      val resultCollectionName = MapReduceMessagesCollectionName + id
      messagesCollection.mapReduce(mapJS, reduceJS, MapReduceStandardOutput(resultCollectionName),
        finalizeFunction = Some(finalizeJS), query = Some(q))

      val order = MongoDBObject("value.createdAt" -> -1)
      val messages = mongo(dataBaseName)(resultCollectionName).find().sort(order).foldLeft(List[JValue]())((l, a) =>
        messageToJSON(a.get("value").asInstanceOf[DBObject]) :: l
      ).reverse
      val json = ("count", messages.size) ~ ("items", messages)
      req.respond(JsonContent ~> ResponseString(pretty(render(json))))

    case req @ GET(Path(Seg("api" :: "v1" :: "categories" :: id :: "message" :: Nil))) =>
      val q = MongoDBObject("categoryId" -> new ObjectId(id), "state" -> "approved")
      val count = messagesCollection.find(q, MongoDBObject("_id" -> 1)).count
      val skip = random.nextInt(if (count <= 0) 1 else count)

      val keys = MongoDBObject("_id" -> 1)
      val messages = messagesCollection.find(q, keys).limit(-1).skip(skip)
      if (!messages.isEmpty) {
        val message = messages.next()

        val q = MongoDBObject("_id" -> message.get("_id"))
        val res = messagesCollection.mapReduce(mapJS, reduceJS, MapReduceInlineOutput,
          finalizeFunction = Some(finalizeJS), query = Some(q)).next()
        val json = messageToJSON(res.get("value").asInstanceOf[DBObject])
        req.respond(JsonContent ~> ResponseString(pretty(render(json))))
      } else {
        req.respond(NoContent)
      }

    case req @ POST(Path(Seg("api" :: "v1" :: "categories" :: categoryId :: "message" :: Nil))) =>
      categoriesCollection.findOne(MongoDBObject("_id" -> new ObjectId(categoryId))).map { category =>
        val Params(form) = req
        val builder = MongoDBObject.newBuilder
        builder += "categoryId" -> category.get("_id")
        builder += "category" -> category.get("name")
        builder += "text" -> form("text")(0)
        form.get("contributorName") match {
          case Some(param) =>
            builder += "contributorName" -> param(0)
          case None =>
            builder += "contributorName" -> ""
        }
        builder += "state" -> "waiting"
        builder += "createdAt" -> new Date()
        builder += "modifiedAt" -> new Date()
        builder += "random" -> scala.math.random
        messagesCollection += builder.result
      }
      req.respond(NoContent)

    case req @ POST(Path(Seg("api" :: "v1" :: "messages" :: messageId :: "rate" :: Nil))) =>
      val Params(form) = req
      val uid = form("uid")(0)
      val rating = if (form("rating")(0).toInt > 4) 4 else form("rating")(0).toInt
      val key = "ratings." + uid.replaceAll("\\.", "-")
      messagesCollection.update(MongoDBObject("_id" -> new ObjectId(messageId)), $set(key -> rating), false, false)
      req.respond(NoContent)
  }

  private def messageToJSON(message: DBObject) = {
    ("id", message.get("_id").toString) ~
      ("type", "message") ~
      ("category", message.get("category").toString) ~
      ("categoryId", message.get("categoryId").toString) ~
      ("text", message.get("text").toString) ~
      ("createdAt", message.get("createdAt").toString) ~
      ("modifiedAt", message.get("modifiedAt").toString) ~
      ("contributorName", message.get("contributorName").toString) ~
      ("rating", message.get("rating").asInstanceOf[Double]) ~
      ("ratingCount", message.get("ratingCount").asInstanceOf[Double].toLong)
  }

  private def categoryToJSON(o: DBObject) = {
    ("id", o.get("_id").toString) ~
      ("type", "category") ~
      ("name", o.get("name").toString) ~
      ("color", o.get("color").toString)
  }

}

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
