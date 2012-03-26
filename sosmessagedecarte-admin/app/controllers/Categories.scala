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

case class Category(name: String, color: String)

object Categories extends Controller {

  val config = SosMessageConfiguration.getConfig

  val CategoriesCollectionName = "categories"
  val MessagesCollectionName = "messages"

  val dataBaseName = config[String]("database.name", "sosmessage")

  val mongo = MongoConnection(config[String]("database.host", "127.0.0.1"), config[Int]("database.port", 27017))

  val categoriesCollection = mongo(dataBaseName)(CategoriesCollectionName)
  val messagesCollection = mongo(dataBaseName)(MessagesCollectionName)

  val categoryForm = Form(
    mapping(
      "name" -> nonEmptyText,
      "color" -> text(minLength = 9)
    )(Category.apply)(Category.unapply)
  )

  def index = Action { implicit request =>
    val categoryOrder = MongoDBObject("name" -> 1)
    val categories = categoriesCollection.find().sort(categoryOrder).foldLeft(List[DBObject]())((l, a) =>
      a :: l
    ).reverse

    val messagesCountByCategory = categories.foldLeft(Map[String, Long]())((m, o) => {
      val count = messagesCollection.count(MongoDBObject("categoryId" -> o.get("_id"), "state" -> "approved"))
      m + (o.get("_id").toString -> count)
    })

    Ok(views.html.categories.index(categories, messagesCountByCategory, categoryForm))
  }

  def save = Action { implicit request =>
    categoryForm.bindFromRequest().fold(
      formWithErrors => {
        Redirect(routes.Categories.index)
      },
      category => {
        val builder = MongoDBObject.newBuilder
        builder += "name" -> category.name
        val color = if (category.color.startsWith("#")) category.color else "#" + category.color
        builder += "color" -> color
        builder += "createdAt" -> new Date()
        builder += "modifiedAt" -> new Date()
        builder += "lastAddedMessageAt" -> new Date()
        categoriesCollection += builder.result

        Redirect(routes.Categories.index).flashing("actionDone" -> "categoryAdded")
      }
    )
  }

  def delete(id: String) = Action { implicit request =>
    val oid = new ObjectId(id)
    val o = MongoDBObject("_id" -> oid)
    categoriesCollection.remove(o)
    Redirect(routes.Categories.index).flashing("actionDone" -> "categoryDeleted")
  }

  def edit(id: String) = Action { implicit request =>
    val q = MongoDBObject("_id" -> new ObjectId(id))
    categoriesCollection.findOne(q).map { category =>
      val c = Category(category.get("name").toString, category.get("color").toString)
      Ok(views.html.categories.edit(id, categoryForm.fill(c)))
    }.getOrElse(NotFound)
  }

  def update(id: String) = Action { implicit request =>
    categoryForm.bindFromRequest.fold(
      formWithErrors => {
        Redirect(routes.Categories.edit(id))
      },
      category => {
        val q = MongoDBObject("_id" -> new ObjectId(id))
        val color = if (category.color.startsWith("#")) category.color else "#" + category.color
        val o = $set("name" -> category.name, "color" -> color, "modifiedAt" -> new Date())
        categoriesCollection.update(q, o, false, false)
        Redirect(routes.Categories.index).flashing("actionDone" -> "categoryUpdated")
      }
    )
  }

}
