package controllers

import play.api._
import data._
import play.api.mvc._
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.MongoConnection
import org.bson.types.ObjectId
import java.util.Date
import com.mongodb.DBObject
import com.mongodb.casbah._
import conf.SosMessageConfiguration

case class Category(name: String,  color: String)

object Categories extends Controller {

  val config = SosMessageConfiguration.getConfig

  val CategoriesCollectionName = "categories"
  val MessagesCollectionName = "messages"

  val dataBaseName = config[String]("database.name", "sosmessage")

  val mongo = MongoConnection(config[String]("database.host", "127.0.0.1"), config[Int]("database.port", 27017))

  val categoriesCollection = mongo(dataBaseName)(CategoriesCollectionName)
  val messagesCollection = mongo(dataBaseName)(MessagesCollectionName)

  val categoryForm = Form(
    of(Category.apply _)(
      "name" -> requiredText,
      "color" -> text (minLength = 9)
    )
  )

  def index = Action { implicit request =>
    val categoryOrder = MongoDBObject("order" -> -1)
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
        builder += "order" -> categoriesCollection.count
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
        val o = $set ("name" -> category.name, "color" -> color, "modifiedAt" -> new Date())
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

  def moveUp(id: String) = Action { implicit request =>
    val categoryOrder = MongoDBObject("order" -> -1)
    val categories = categoriesCollection.find().sort(categoryOrder).foldLeft(List[DBObject]())((l, a) =>
      a :: l
    ).reverse

    categories.find(o => id == o.get("_id").toString).map { selectedCategory =>
      val index = categories.indexOf(selectedCategory)
      if (index > 0) {
        var q = MongoDBObject("_id" -> selectedCategory.get("_id"))
        var o = $inc ("order" -> 1)
        categoriesCollection.update(q, o, false, false)

        var categoryBefore = categories(index - 1)
        q = MongoDBObject("_id" -> categoryBefore.get("_id"))
        o = $inc ("order" -> -1)
        categoriesCollection.update(q, o, false, false)
      }
    }

    Redirect(routes.Categories.index).flashing("actionDone" -> "categoryUpdated")
  }

  def moveDown(id: String) = Action { implicit request =>
    val categoryOrder = MongoDBObject("order" -> -1)
    val categories = categoriesCollection.find().sort(categoryOrder).foldLeft(List[DBObject]())((l, a) =>
      a :: l
    ).reverse

    categories.find(o => id == o.get("_id").toString).map { selectedCategory =>
      val index = categories.indexOf(selectedCategory)
      if (index < categories.size - 1) {
        var q = MongoDBObject("_id" -> selectedCategory.get("_id"))
        var o = $inc ("order" -> -1)
        categoriesCollection.update(q, o, false, false)

        var categoryAfter = categories(index + 1)
        q = MongoDBObject("_id" -> categoryAfter.get("_id"))
        o = $inc ("order" -> 1)
        categoriesCollection.update(q, o, false, false)
      }
    }
    Redirect(routes.Categories.index).flashing("actionDone" -> "categoryUpdated")
  }

}
