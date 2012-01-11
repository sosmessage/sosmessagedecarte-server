package controllers

import play.api.mvc._
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.MongoConnection
import org.bson.types.ObjectId
import com.mongodb.casbah._
import conf.SosMessageConfiguration
import java.util.Date

object Moderation extends Controller {

  val config = SosMessageConfiguration.getConfig

  val CategoriesCollectionName = "categories"
  val MessagesCollectionName = "messages"

  val dataBaseName = config[String]("database.name", "sosmessage")

  val mongo = MongoConnection(config[String]("database.host", "127.0.0.1"), config[Int]("database.port", 27017))

  val categoriesCollection = mongo(dataBaseName)(CategoriesCollectionName)
  val messagesCollection = mongo(dataBaseName)(MessagesCollectionName)

  def index(state: String = "waiting") = Action { implicit request =>
    val messageOrder = MongoDBObject("createdAt" -> -1)
    val q = MongoDBObject("state" -> state)
    val messages = messagesCollection.find(q).sort(messageOrder).foldLeft(List[DBObject]())((l, a) =>
      a :: l
    ).reverse
    Ok(views.html.moderation.index(state, messages))
  }

  def approve(messageId: String, selectedTab: String) = Action { implicit request =>
    val oid = new ObjectId(messageId)
    var message = messagesCollection.findOne(MongoDBObject("_id" -> oid)).get
    message += ("state" -> "approved")
    messagesCollection.save(message)

    val q = MongoDBObject("_id" -> message.get("categoryId"))
    val o = $set("lastAddedMessageAt" -> new Date())
    categoriesCollection.update(q, o, false, false)

    Redirect(routes.Moderation.index(selectedTab)).flashing("actionDone" -> "messageApproved")
  }

  def reject(messageId: String, selectedTab: String) = Action { implicit request =>
    val oid = new ObjectId(messageId)
    var o = messagesCollection.findOne(MongoDBObject("_id" -> oid)).get
    o += ("state" -> "rejected")
    messagesCollection.save(o)
    Redirect(routes.Moderation.index(selectedTab)).flashing("actionDone" -> "messageRejected")
  }

  def delete(messageId: String, selectedTab: String) = Action { implicit request =>
    val oid = new ObjectId(messageId)
    val o = MongoDBObject("_id" -> oid)
    messagesCollection.remove(o)
    Redirect(routes.Moderation.index(selectedTab)).flashing("actionDone" -> "messageDeleted")
  }

  def deleteAll(state: String) = Action { implicit request =>
    val o = MongoDBObject("state" -> state)
    messagesCollection.remove(o)
    Redirect(routes.Moderation.index("waiting")).flashing("actionDone" -> (state + "MessagesDeleted"))
  }

  def approveAll(state: String) = Action { implicit request =>
    val keys = MongoDBObject("categoryId" -> 1)
    val categoryIds = messagesCollection.find(MongoDBObject("state" -> state), keys).foldLeft(List[ObjectId]())((l, a) =>
      a.get("categoryId").asInstanceOf[ObjectId] :: l
    ).distinct
    categoryIds.map { id =>
      val q = MongoDBObject("_id" -> id)
      val o = $set("lastAddedMessageAt" -> new Date())
      categoriesCollection.update(q, o, false, false)
    }

    messagesCollection.update(MongoDBObject("state" -> state), $set("state" -> "approved"), false, true)
    Redirect(routes.Moderation.index("waiting")).flashing("actionDone" -> (state + "MessagesApproved"))
  }
  def rejectAll(state: String) = Action { implicit request =>
    messagesCollection.update(MongoDBObject("state" -> state), $set("state" -> "rejected"), false, true)
    Redirect(routes.Moderation.index("waiting")).flashing("actionDone" -> (state + "MessagesRejected"))
  }

}
