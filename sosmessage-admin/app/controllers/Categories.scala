package controllers

import play.api._
import data._
import play.api.mvc._
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.MongoConnection
import org.bson.types.ObjectId
import java.util.Date
import com.mongodb.DBObject
import com.mongodb.casbah.Imports._

object Categories extends Controller {

  val DataBaseName = "sosmessage"
  val CategoriesCollectionName = "categories"
  val MessagesCollectionName = "messages"

  val mongo = MongoConnection()

  val categoriesCollection = mongo(DataBaseName)(CategoriesCollectionName)
  val messagesCollection = mongo(DataBaseName)(MessagesCollectionName)

  val categoryForm = Form(
      "name" -> text(minLength = 1)
  )

  def index = Action { implicit request =>
    val categoryOrder = MongoDBObject("name" -> 1)
    val categories = categoriesCollection.find().sort(categoryOrder).foldLeft(List[DBObject]())((l, a) =>
      a :: l
    ).reverse

    val messagesCountByCategory = categories.foldLeft(Map[String, Long]())((m, o) => {
      val count = messagesCollection.count(MongoDBObject("categoryId" -> o.get("_id")))
      m + (o.get("_id").toString -> count)
    }
    )

    Ok(views.html.categories.index(categories, messagesCountByCategory, categoryForm))
  }

  def save = Action { implicit request =>
    categoryForm.bindFromRequest().fold(
      f => {
        Redirect(routes.Categories.index)
      },
      v => {
        val builder = MongoDBObject.newBuilder
        builder += "name" -> v
        builder += "createdAt" -> new Date()
        builder += "modifiedAt" -> new Date()
        categoriesCollection += builder.result

        Redirect(routes.Categories.index).flashing("actionDone" -> "categoryAdded")
      }
    )
  }

  def delete (id: String) = Action { implicit request =>
    val oid = new ObjectId(id)
    val o = MongoDBObject("_id" -> oid)
    categoriesCollection.remove(o)
    Redirect(routes.Categories.index).flashing("actionDone" -> "categoryDeleted")
  }

  def edit(id: String) = Action { implicit request =>
    val q = MongoDBObject("_id" -> new ObjectId(id))
    categoriesCollection.findOne(q).map { category =>
      Ok(views.html.categories.edit(id, categoryForm.fill(category.get("name").toString)))
    }.getOrElse(NotFound)
  }

  def update(id: String) = Action { implicit request =>
    categoryForm.bindFromRequest.fold(
      f => {
        Redirect(routes.Categories.index)
      },
      v => {
        val q = MongoDBObject("_id" -> new ObjectId(id))
        val o = $set ("name" -> v, "modifiedAt" -> new Date())
        categoriesCollection.update(q, o, false, false)
        Redirect(routes.Categories.index).flashing("actionDone" -> "categoryUpdated")
      }
    )
  }

  def publish(id: String) = Action { implicit request =>
    val oid = new ObjectId(id)
    var o = categoriesCollection.findOne(MongoDBObject("_id" -> oid)).get
    o += ("published" -> (true: java.lang.Boolean))
    categoriesCollection.save(o)
    Redirect(routes.Categories.index).flashing("actionDone" -> "categoryUpdated")
  }

  def unpublish(id: String) = Action { implicit request =>
    val oid = new ObjectId(id)
    var o = categoriesCollection.findOne(MongoDBObject("_id" -> oid)).get
    o += ("published" -> (false: java.lang.Boolean))
    categoriesCollection.save(o)
    Redirect(routes.Categories.index).flashing("actionDone" -> "categoryUpdated")
  }

}
