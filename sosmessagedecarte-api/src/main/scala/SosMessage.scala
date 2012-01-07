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
import javax.mail._
import javax.mail.internet._
import scala.actors.Actor._

case class SendEmail(message: DBObject)

class SosMessage(config: Configuration) extends async.Plan with ServerErrorResponse {

  val MessagesCollectionName = "messages"
  val CategoriesCollectionName = "categories"
  val MapReduceMessagesCollectionName = "mapReduceMessages_"

  val DefaultSosMessageAppName = "smdc"

  val dataBaseName = config[String]("database.name", "sosmessage")

  val mongo = MongoConnection(config[String]("database.host", "127.0.0.1"), config[Int]("database.port", 27017))
  val messagesCollection = mongo(dataBaseName)(MessagesCollectionName)
  val categoriesCollection = mongo(dataBaseName)(CategoriesCollectionName)

  val random = new Random()

  private val emailSender = actor {
    loop {
      react {
        case SendEmail(message) =>
          val tls = config[String]("mail.tls", "true")
          val host = config[String]("mail.host")
          val port = config[Int]("mail.port")
          val user = config[String]("mail.user")
          val password = config[String]("mail.password")

          val props = System.getProperties
          props.put("mail.smtp.auth", "true");
          props.put("mail.smtp.starttls.enable", tls);
          props.put("mail.smtp.host", host);
          props.put("mail.smtp.user", user);
          props.put("mail.smtp.password", password);
          props.put("mail.smtp.port", port.toString);
          val session = Session.getDefaultInstance(props)
          val mimeMessage = new MimeMessage(session)

          mimeMessage.setFrom(new InternetAddress(config[String]("mail.from")))
          mimeMessage.setRecipients(Message.RecipientType.TO, config[String]("mail.recipients"))
          mimeMessage.setSubject("[Moderation] New message waiting for approval")
          val text = """
            Hi,

            There is a new message waiting your approval!

            Category:
              %s

            Message:
              %s

            Contributed by %s
          """.format(message.get("category").toString, message.get("text").toString, message.get("contributorName").toString)
          mimeMessage.setText(text)

          val transport = session.getTransport("smtp");
          transport.connect(host, port, user, password);
          transport.sendMessage(mimeMessage, mimeMessage.getAllRecipients());
          transport.close();
      }
    }
  }

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
      var votePlus = 0;
      var voteMinus = 0;
      var userVote = 0;
      for (var prop in value.ratings) {
        var rating = value.ratings[prop];
        var vote = rating == 1 ? -1 : 1
        if (prop == uid) {
          userVote = vote
        }

        if (vote == 1) {
          votePlus++;
        } else {
          voteMinus++;
        }

        count++;
        total += value.ratings[prop];
      }

      value.votePlus = votePlus;
      value.voteMinus = voteMinus;
      value.userVote = userVote;

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
      val Params(form) = req
      val appName = form.get("appname") match {
        case Some(params) => params(0)
        case None => DefaultSosMessageAppName
      }

      val categoryOrder = MongoDBObject("apps." + appName + ".order" -> -1)
      val q = MongoDBObject("apps." + appName + ".published" -> true)
      val categories = categoriesCollection.find(q).sort(categoryOrder).foldLeft(List[JValue]())((l, a) =>
        categoryToJSON(a) :: l
      ).reverse
      val json = ("count", categories.size) ~ ("items", categories)
      req.respond(JsonContent ~> ResponseString(pretty(render(json))))

    case req @ GET(Path(Seg("api" :: "v1" :: "categories" :: id :: "messages" :: Nil))) =>
      val Params(form) = req
      val jsScope = form.get("uid") match {
        case Some(param) => Some(MongoDBObject("uid" -> param))
        case None => Some(MongoDBObject("uid" -> ""))
      }

      val q = MongoDBObject("categoryId" -> new ObjectId(id), "state" -> "approved")
      val resultCollectionName = MapReduceMessagesCollectionName + id
      messagesCollection.mapReduce(mapJS, reduceJS, MapReduceStandardOutput(resultCollectionName),
        finalizeFunction = Some(finalizeJS), query = Some(q), jsScope = jsScope)

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

        val Params(form) = req
        val jsScope = form.get("uid") match {
          case Some(param) => Some(MongoDBObject("uid" -> param))
          case None => Some(MongoDBObject("uid" -> ""))
        }

        val q = MongoDBObject("_id" -> message.get("_id"))
        val res = messagesCollection.mapReduce(mapJS, reduceJS, MapReduceInlineOutput,
          finalizeFunction = Some(finalizeJS), query = Some(q), jsScope = jsScope).next()
        val json = messageToJSON(res.get("value").asInstanceOf[DBObject])
        req.respond(JsonContent ~> ResponseString(pretty(render(json))))
      } else {
        req.respond(NoContent)
      }

    case req @ POST(Path(Seg("api" :: "v1" :: "categories" :: categoryId :: "message" :: Nil))) =>
      categoriesCollection.findOne(MongoDBObject("_id" -> new ObjectId(categoryId))) match {
        case Some(category) =>
          val Params(form) = req
          form.get("text") match {
            case Some(textParam) =>
              val builder = MongoDBObject.newBuilder
              builder += "categoryId" -> category.get("_id")
              builder += "category" -> category.get("name")
              builder += "text" -> textParam(0)
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
              val result = builder.result
              messagesCollection += result

              emailSender ! SendEmail(result)

              req.respond(NoContent)

            case None => req.respond(BadRequest)
          }
        case None => req.respond(BadRequest)
      }

    case req @ POST(Path(Seg("api" :: "v1" :: "messages" :: messageId :: "rate" :: Nil))) =>
      val Params(form) = req
      if (!form.contains("uid") || !form.contains("rating")) {
        req.respond(BadRequest)
      } else {
        val uid = form("uid")(0)
        val rating = if (form("rating")(0).toInt > 5) 5 else form("rating")(0).toInt
        val key = "ratings." + uid.replaceAll("\\.", "-")
        messagesCollection.update(MongoDBObject("_id" -> new ObjectId(messageId)), $set(key -> rating), false, false)
        req.respond(NoContent)
      }

    case req @ POST(Path(Seg("api" :: "v1" :: "messages" :: messageId :: "vote" :: Nil))) =>
      val Params(form) = req
      if (!form.contains("uid") || !form.contains("vote")) {
        req.respond(BadRequest)
      } else {
        val uid = form("uid")(0)
        val vote = form("vote")(0).toInt
        if (vote != 1 && vote != -1) {
          req.respond(BadRequest)
        } else {
          val rating = if (vote == 1) 5 else 1
          val key = "ratings." + uid.replaceAll("\\.", "-")
          val q = MongoDBObject("_id" -> new ObjectId(messageId))
          messagesCollection.update(q, $set(key -> rating), false, false)

          val jsScope = Some(MongoDBObject("uid" -> uid))
          val res = messagesCollection.mapReduce(mapJS, reduceJS, MapReduceInlineOutput,
            finalizeFunction = Some(finalizeJS), query = Some(q), jsScope = jsScope).next()
          val json = messageToJSON(res.get("value").asInstanceOf[DBObject])
          req.respond(JsonContent ~> ResponseString(pretty(render(json))))
        }
      }
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
      ("vote", ("plus", message.get("votePlus").asInstanceOf[Double].toLong) ~ ("minus", message.get("voteMinus").asInstanceOf[Double].toLong) ~ ("userVote", message.get("userVote").asInstanceOf[Double].toLong)) ~
      ("rating", ("count", message.get("ratingCount").asInstanceOf[Double].toLong) ~ ("value", message.get("rating").asInstanceOf[Double]))
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
