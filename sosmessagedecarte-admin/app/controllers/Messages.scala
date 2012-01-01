package controllers

import _root_.net.liftweb.json.JsonParser._
import play.api._
import data._
import play.api.mvc._
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.MongoConnection
import org.bson.types.ObjectId
import java.util.Date
import com.mongodb.DBObject
import com.mongodb.casbah.Imports._

case class Message(categoryId: String, text: String, contributorName: String, 
                   contributorEmail: String, approved: Option[String])

object Messages extends Controller {

  val DataBaseName = "sosmessage"
  val MessagesCollectionName = "messages"
  val CategoriesCollectionName = "categories"

  val mongo = MongoConnection()

  val messagesCollection = mongo(DataBaseName)(MessagesCollectionName)
  val categoriesCollection = mongo(DataBaseName)(CategoriesCollectionName)
  
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

    val messageOrder = MongoDBObject("createdAt" -> -1)
    val q = MongoDBObject("categoryId" -> new ObjectId(selectedCategoryId), "state" -> "approved")
    val messages = messagesCollection.find(q).sort(messageOrder).foldLeft(List[DBObject]())((l, a) =>
      a :: l
    ).map(addRating(_)).reverse

    Ok(views.html.messages.index(categories, selectedCategoryId, messages, messageForm))
  }

  def addRating(message: DBObject) = {
    val r = """
    function(doc, out) {
      for (var prop in doc.ratings) {
        out.count++;
        out.total += doc.ratings[prop];
      }
    }
    """
    val f = """
    function(out) {
      if (out.count == 0) {
        out.avg = 0;
      } else {
        out.avg = out.total / out.count;
      }
    }
    """
    val rating = messagesCollection.group(MongoDBObject("ratings" -> 1),
      MongoDBObject("_id" -> message.get("_id")), MongoDBObject("count" -> 0, "total" -> 0), r, f)
    val j = parse(rating.mkString)
    message.put("rating", j \ "avg" values)
    message.put("ratingCount", (j \ "count" values).asInstanceOf[Double].toInt)
    message
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
