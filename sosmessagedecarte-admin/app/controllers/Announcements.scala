package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.mvc._
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.MongoConnection
import org.bson.types.ObjectId
import java.util.Date
import com.mongodb.DBObject
import com.mongodb.casbah._
import conf.SosMessageConfiguration

case class Announcement(title: String, text: String, url: String, cancelButton: String, validateButton: String)

object Announcements extends Controller {

  val config = SosMessageConfiguration.getConfig

  val AnnouncementsCollectionName = "announcements"

  val dataBaseName = config[String]("database.name", "sosmessage")

  val mongo = MongoConnection(config[String]("database.host", "127.0.0.1"), config[Int]("database.port", 27017))

  val announcementsCollection = mongo(dataBaseName)(AnnouncementsCollectionName)

  val announcementForm = Form(
    mapping(
      "title" -> nonEmptyText,
      "text" -> nonEmptyText,
      "url" -> text,
      "validateButton" -> text,
      "cancelButton" -> text
    )(Announcement.apply)(Announcement.unapply)
  )

  def index = Action { implicit request =>
    val announcementOrder = MongoDBObject("title" -> 1)
    val announcements = announcementsCollection.find().sort(announcementOrder).foldLeft(List[DBObject]())((l, a) =>
      a :: l
    ).reverse
    Ok(views.html.announcements.index(announcements, announcementForm))
  }

  def save = Action { implicit request =>
    announcementForm.bindFromRequest().fold(
      formWithErrors => {
        Redirect(routes.Announcements.index)
      },
      announcement => {
        val builder = MongoDBObject.newBuilder
        builder += "title" -> announcement.title
        builder += "text" -> announcement.text
        builder += "url" -> announcement.url
        builder += "buttons" -> MongoDBObject("validate" -> announcement.validateButton, "cancel" -> announcement.cancelButton)
        builder += "createdAt" -> new Date()
        builder += "modifiedAt" -> new Date()
        announcementsCollection += builder.result

        Redirect(routes.Announcements.index).flashing("actionDone" -> "announcementAdded")
      }
    )
  }

  def delete(id: String) = Action { implicit request =>
    val oid = new ObjectId(id)
    val o = MongoDBObject("_id" -> oid)
    announcementsCollection.remove(o)
    Redirect(routes.Announcements.index).flashing("actionDone" -> "announcementDeleted")
  }

  def edit(id: String) = Action { implicit request =>
    val q = MongoDBObject("_id" -> new ObjectId(id))
    announcementsCollection.findOne(q).map { announcement =>
      val a = Announcement(announcement.get("title").toString, announcement.get("text").toString,
        announcement.get("url").toString, announcement.get("buttons").asInstanceOf[DBObject].get("validate").toString,
        announcement.get("buttons").asInstanceOf[DBObject].get("cancel").toString)
      Ok(views.html.announcements.edit(id, announcementForm.fill(a)))
    }.getOrElse(NotFound)
  }

  def update(id: String) = Action { implicit request =>
    announcementForm.bindFromRequest.fold(
      formWithErrors => {
        Redirect(routes.Announcements.edit(id))
      },
      announcement => {
        val q = MongoDBObject("_id" -> new ObjectId(id))

        val o = $set("title" -> announcement.title, "text" -> announcement.text, "url" -> announcement.url,
          "buttons" -> MongoDBObject("validate" -> announcement.validateButton, "cancel" -> announcement.cancelButton),
          "modifiedAt" -> new Date())
        announcementsCollection.update(q, o, false, false)
        Redirect(routes.Announcements.index).flashing("actionDone" -> "announcementUpdated")
      }
    )
  }

}
