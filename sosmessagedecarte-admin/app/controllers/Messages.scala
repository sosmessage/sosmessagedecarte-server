package controllers

import play.api._
import data._
import play.api.mvc._
import com.mongodb.casbah.commons.MongoDBObject
import org.bson.types.ObjectId
import java.util.Date
import com.mongodb.DBObject
import com.mongodb.casbah._
import map_reduce.MapReduceStandardOutput
import conf.SosMessageConfiguration
import com.mongodb.casbah.MongoConnection._

case class Message(categoryId: String, text: String, contributorName: String,
                   contributorEmail: String, approved: Option[String])

object Messages extends Controller {

  val config = SosMessageConfiguration.getConfig

  val CategoriesCollectionName = "categories"
  val MessagesCollectionName = "messages"
  val MapReduceMessagesCollectionName = "mapReduceMessages_"

  val dataBaseName = config[String]("database.name", "sosmessage")

  val mongo = MongoConnection(config[String]("database.host", "127.0.0.1"), config[Int]("database.port", 27017))

  val categoriesCollection = mongo(dataBaseName)(CategoriesCollectionName)
  val messagesCollection = mongo(dataBaseName)(MessagesCollectionName)

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

  val messageForm = Form(
    of(Message.apply _)(
      "categoryId" -> requiredText,
      "text" -> requiredText,
      "contributorName" -> text,
      "contributorEmail" -> text,
      "approved" -> optional(text)
    )
  )

  def index(categoryId: Option[String] = None) = Action { implicit request =>
    val categoryOrder = MongoDBObject("name" -> 1)
    val categories = categoriesCollection.find().sort(categoryOrder).foldLeft(List[DBObject]())((l, a) =>
      a :: l
    ).reverse

    val selectedCategoryId = categoryId.getOrElse(categories(0).get("_id").toString)

    val q = MongoDBObject("categoryId" -> new ObjectId(selectedCategoryId), "state" -> "approved")
    val resultCollectionName = MapReduceMessagesCollectionName + selectedCategoryId
    messagesCollection.mapReduce(mapJS, reduceJS, MapReduceStandardOutput(resultCollectionName),
      finalizeFunction = Some(finalizeJS), query = Some(q))

    val messageOrder = MongoDBObject("value.createdAt" -> -1)
    val messages = mongo(dataBaseName)(resultCollectionName).find().sort(messageOrder).foldLeft(List[DBObject]())((l, a) =>
      a.get("value").asInstanceOf[DBObject] :: l
    ).reverse

    Ok(views.html.messages.index(categories, selectedCategoryId, messages, messageForm))
  }

  def save(selectedCategoryId: String) = Action { implicit request =>
    messageForm.bindFromRequest().fold(
      formWithErrors => {
        Redirect(routes.Messages.index(Some(selectedCategoryId)))
      },
      message => {
        val oid = new ObjectId(message.categoryId)
        val o = MongoDBObject("_id" -> oid)
        val category = categoriesCollection.findOne(o).get
        val builder = MongoDBObject.newBuilder
        builder += "categoryId" -> category.get("_id")
        builder += "category" -> category.get("name")
        builder += "text" -> message.text
        builder += "contributorName" -> message.contributorName
        builder += "contributorEmail" -> message.contributorEmail
        val actionDone = message.approved match {
          case None =>
            builder += "state" -> "waiting"
            "messageWaiting"
          case Some(s) =>
            builder += "state" -> "approved"
            "messageAdded"
        }
        builder += "createdAt" -> new Date()
        builder += "modifiedAt" -> new Date()
        builder += "random" -> scala.math.random
        messagesCollection += builder.result

        Redirect(routes.Messages.index(Some(category.get("_id").toString))).flashing("actionDone" -> actionDone)
      }
    )
  }

  def delete(selectedCategoryId: String, messageId: String) = Action { implicit request =>
    val oid = new ObjectId(messageId)
    val o = MongoDBObject("_id" -> oid)
    messagesCollection.remove(o)
    Redirect(routes.Messages.index(Some(selectedCategoryId))).flashing("actionDone" -> "messageDeleted")
  }

  def edit(categoryId: String, messageId: String) = Action { implicit request =>
    val categoryOrder = MongoDBObject("name" -> 1)
    val categories = categoriesCollection.find().sort(categoryOrder).foldLeft(List[DBObject]())((l, a) =>
      a :: l
    ).reverse
    val q = MongoDBObject("_id" -> new ObjectId(messageId))
    messagesCollection.findOne(q).map { message =>
      val m = Message(message.get("categoryId").toString, message.get("text").toString,
        message.get("contributorName").toString, message.get("contributorEmail").toString, None)
      Ok(views.html.messages.edit(categories, categoryId, messageId, messageForm.fill(m)))
    }.getOrElse(NotFound)
  }

  def update(categoryId: String, messageId: String) = Action { implicit request =>
    messageForm.bindFromRequest.fold(
      formWithErrors => {
        Redirect(routes.Messages.index(Some(categoryId)))
      },
      message => {
        val newCategoryId = message.categoryId
        val q = MongoDBObject("_id" -> new ObjectId(messageId))
        val o = $set ("categoryId" -> new ObjectId(newCategoryId), "text" -> message.text,
          "contributorName" -> message.contributorName, "contributorEmail" -> message.contributorEmail,
          "modifiedAt" -> new Date())
        messagesCollection.update(q, o, false, false)
        Redirect(routes.Messages.index(Some(newCategoryId))).flashing("actionDone" -> "messageUpdated")
      }
    )
  }

}
