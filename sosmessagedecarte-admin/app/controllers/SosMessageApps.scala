package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.mvc._
import conf.SosMessageConfiguration
import com.mongodb.casbah._
import com.mongodb.DBObject
import java.util.Date

case class SosMessageApp(name: String, title: String)

case class NewAnnouncement(id: String)
case class NewCategory(id: String)

object SosMessageApps extends Controller {

  val config = SosMessageConfiguration.getConfig

  val AnnouncementsCollectionName = "announcements"
  val CategoriesCollectionName = "categories"
  val MessagesCollectionName = "messages"
  val AppsCollectionName = "sosmessageapps"

  val dataBaseName = config[String]("database.name", "sosmessage")

  val mongo = MongoConnection(config[String]("database.host", "127.0.0.1"), config[Int]("database.port", 27017))

  val announcementsCollection = mongo(dataBaseName)(AnnouncementsCollectionName)
  val categoriesCollection = mongo(dataBaseName)(CategoriesCollectionName)
  val messagesCollection = mongo(dataBaseName)(MessagesCollectionName)
  val appsCollection = mongo(dataBaseName)(AppsCollectionName)

  val appForm = Form(
    mapping(
      "name" -> nonEmptyText,
      "title" -> nonEmptyText
    )(SosMessageApp.apply)(SosMessageApp.unapply)
  )

  val addCategoryForm = Form(
    mapping(
      "id" -> nonEmptyText
    )(NewCategory.apply)(NewCategory.unapply)
  )

  val addAnnouncementForm = Form(
    mapping(
      "id" -> nonEmptyText
    )(NewAnnouncement.apply)(NewAnnouncement.unapply)
  )

  def index = Action { implicit request =>
    val apps = appsCollection.find().foldLeft(List[DBObject]())((l, a) =>
      a :: l
    ).reverse
    Ok(views.html.apps.index(apps, appForm))
  }

  def save = Action { implicit request =>
    appForm.bindFromRequest().fold(
      formWithErrors => {
        Redirect(routes.SosMessageApps.index)
      },
      app => {
        val builder = MongoDBObject.newBuilder
        builder += "name" -> app.name
        builder += "title" -> app.title
        builder += "createdAt" -> new Date()
        builder += "modifiedAt" -> new Date()
        appsCollection += builder.result

        Redirect(routes.SosMessageApps.index).flashing("actionDone" -> "appAdded")
      }
    )
  }

  def delete(id: String) = Action { implicit request =>
    appsCollection.findOne(MongoDBObject("_id" -> new ObjectId(id))).map { app =>
      val key = "apps." + app.get("name")
      val o = $unset(key)
      val q = MongoDBObject(key -> MongoDBObject("$exists" -> true))
      categoriesCollection.update(q, o, false, true)

      appsCollection.remove(MongoDBObject("_id" -> new ObjectId(id)))
    }
    Redirect(routes.SosMessageApps.index).flashing("actionDone" -> "appDeleted")
  }

  def categories(appId: String) = Action { implicit request =>
    val q = MongoDBObject("_id" -> new ObjectId(appId))
    appsCollection.findOne(q).map { app =>
      val appCategoriesQuery = MongoDBObject("apps." + app.get("name").toString -> MongoDBObject("$exists" -> true))
      val categoryOrder = MongoDBObject("apps." + app.get("name").toString + ".order" -> -1)
      val appCategories = categoriesCollection.find(appCategoriesQuery).sort(categoryOrder).foldLeft(List[DBObject]())((l, a) =>
        a :: l
      ).reverse

      val nonAppCategoriesQuery = MongoDBObject("apps." + app.get("name").toString -> MongoDBObject("$exists" -> false))
      val nonAppCategories = categoriesCollection.find(nonAppCategoriesQuery).foldLeft(List[DBObject]())((l, a) =>
        a :: l
      ).reverse

      val messagesCountByCategory = appCategories.foldLeft(Map[String, Long]())((m, o) => {
        val count = messagesCollection.count(MongoDBObject("categoryId" -> o.get("_id")))
        m + (o.get("_id").toString -> count)
      })
      Ok(views.html.apps.categories(app, appCategories, messagesCountByCategory, nonAppCategories, addCategoryForm))
    }.getOrElse(NotFound)
  }

  def addCategory(appId: String) = Action { implicit request =>
    addCategoryForm.bindFromRequest().fold(
      formWithErrors => {
        Redirect(routes.SosMessageApps.categories(appId))
      },
      newCategory => {
        appsCollection.findOne(MongoDBObject("_id" -> new ObjectId(appId))).map { app =>
          val appCategoriesQuery = MongoDBObject("apps." + app.get("name").toString -> MongoDBObject("$exists" -> true))
          val appCategories = categoriesCollection.find(appCategoriesQuery)

          val q = MongoDBObject("_id" -> new ObjectId(newCategory.id))
          val key = "apps." + app.get("name")
          val o = $set(key -> MongoDBObject("published" -> false, "order" -> appCategories.count), "modifiedAt" -> new Date())
          categoriesCollection.update(q, o, false, false)
          Redirect(routes.SosMessageApps.categories(appId)).flashing("actionDone" -> "categoryAdded")
        }
        Redirect(routes.SosMessageApps.categories(appId))
      }
    )
  }

  def removeCategory(appId: String, categoryId: String) = Action { implicit request =>
    appsCollection.findOne(MongoDBObject("_id" -> new ObjectId(appId))).map { app =>
      val q = MongoDBObject("_id" -> new ObjectId(categoryId))
      val key = "apps." + app.get("name")
      val o = $unset(key)
      categoriesCollection.update(q, o, false, false)
      Redirect(routes.SosMessageApps.categories(appId)).flashing("actionDone" -> "categoryRemoved")
    }
    Redirect(routes.SosMessageApps.categories(appId))
  }

  def publishCategory(appId: String, categoryId: String) = Action { implicit request =>
    val q = MongoDBObject("_id" -> new ObjectId(appId))
    appsCollection.findOne(q).map { app =>
      val key = "apps." + app.get("name") + ".published"
      val o = $set(key -> true, "modifiedAt" -> new Date())
      val q = MongoDBObject("_id" -> new ObjectId(categoryId))
      categoriesCollection.update(q, o, false, false)
    }
    Redirect(routes.SosMessageApps.categories(appId)).flashing("actionDone" -> "categoryUpdated")
  }

  def unpublishCategory(appId: String, categoryId: String) = Action { implicit request =>
    val q = MongoDBObject("_id" -> new ObjectId(appId))
    appsCollection.findOne(q).map { app =>
      val key = "apps." + app.get("name") + ".published"
      val o = $set(key -> false, "modifiedAt" -> new Date())
      val q = MongoDBObject("_id" -> new ObjectId(categoryId))
      categoriesCollection.update(q, o, false, false)
    }
    Redirect(routes.SosMessageApps.categories(appId)).flashing("actionDone" -> "categoryUpdated")
  }

  def moveCategoryUp(appId: String, categoryId: String) = Action { implicit request =>
    val q = MongoDBObject("_id" -> new ObjectId(appId))
    appsCollection.findOne(q).map { app =>
      val appCategoriesQuery = MongoDBObject("apps." + app.get("name").toString -> MongoDBObject("$exists" -> true))
      val categoryOrder = MongoDBObject("apps." + app.get("name").toString + ".order" -> -1)
      val appCategories = categoriesCollection.find(appCategoriesQuery).sort(categoryOrder).foldLeft(List[DBObject]())((l, a) =>
        a :: l
      ).reverse

      appCategories.find(o => categoryId == o.get("_id").toString).map { selectedCategory =>
        val index = appCategories.indexOf(selectedCategory)
        if (index > 0) {
          val key = "apps." + app.get("name").toString + ".order"
          var q = MongoDBObject("_id" -> selectedCategory.get("_id"))
          var o = $inc(key -> 1)
          categoriesCollection.update(q, o, false, false)

          var categoryBefore = appCategories(index - 1)
          q = MongoDBObject("_id" -> categoryBefore.get("_id"))
          o = $inc(key -> -1)
          categoriesCollection.update(q, o, false, false)
        }
      }
    }
    Redirect(routes.SosMessageApps.categories(appId)).flashing("actionDone" -> "categoryUpdated")
  }

  def moveCategoryDown(appId: String, categoryId: String) = Action { implicit request =>
    val q = MongoDBObject("_id" -> new ObjectId(appId))
    appsCollection.findOne(q).map { app =>
      val appCategoriesQuery = MongoDBObject("apps." + app.get("name").toString -> MongoDBObject("$exists" -> true))
      val categoryOrder = MongoDBObject("apps." + app.get("name").toString + ".order" -> -1)
      val appCategories = categoriesCollection.find(appCategoriesQuery).sort(categoryOrder).foldLeft(List[DBObject]())((l, a) =>
        a :: l
      ).reverse

      appCategories.find(o => categoryId == o.get("_id").toString).map { selectedCategory =>
        val index = appCategories.indexOf(selectedCategory)
        if (index < appCategories.size - 1) {
          val key = "apps." + app.get("name").toString + ".order"
          var q = MongoDBObject("_id" -> selectedCategory.get("_id"))
          var o = $inc(key -> -1)
          categoriesCollection.update(q, o, false, false)

          var categoryAfter = appCategories(index + 1)
          q = MongoDBObject("_id" -> categoryAfter.get("_id"))
          o = $inc(key -> 1)
          categoriesCollection.update(q, o, false, false)
        }
      }
    }
    Redirect(routes.SosMessageApps.categories(appId)).flashing("actionDone" -> "categoryUpdated")
  }

  def announcements(appId: String) = Action { implicit request =>
    val q = MongoDBObject("_id" -> new ObjectId(appId))
    appsCollection.findOne(q).map { app =>
      val appAnnouncementsQuery = MongoDBObject("apps." + app.get("name").toString -> MongoDBObject("$exists" -> true))
      val announcementOrder = MongoDBObject("apps." + app.get("name").toString + ".order" -> -1)
      val appAnnouncements = announcementsCollection.find(appAnnouncementsQuery).sort(announcementOrder).foldLeft(List[DBObject]())((l, a) =>
        a :: l
      ).reverse

      val nonAppAnnouncementsQuery = MongoDBObject("apps." + app.get("name").toString -> MongoDBObject("$exists" -> false))
      val nonAppAnnouncements = announcementsCollection.find(nonAppAnnouncementsQuery).foldLeft(List[DBObject]())((l, a) =>
        a :: l
      ).reverse

      Ok(views.html.apps.announcements(app, appAnnouncements, nonAppAnnouncements, addAnnouncementForm))
    }.getOrElse(NotFound)
  }

  def addAnnouncement(appId: String) = Action { implicit request =>
    addAnnouncementForm.bindFromRequest().fold(
      formWithErrors => {
        Redirect(routes.SosMessageApps.announcements(appId))
      },
      newAnnouncement => {
        appsCollection.findOne(MongoDBObject("_id" -> new ObjectId(appId))).map { app =>
          val appAnnouncementsQuery = MongoDBObject("apps." + app.get("name").toString -> MongoDBObject("$exists" -> true))
          val appAnnouncements = announcementsCollection.find(appAnnouncementsQuery)

          val q = MongoDBObject("_id" -> new ObjectId(newAnnouncement.id))
          val key = "apps." + app.get("name")
          val o = $set(key -> MongoDBObject("published" -> false), "modifiedAt" -> new Date())
          announcementsCollection.update(q, o, false, false)
          Redirect(routes.SosMessageApps.announcements(appId)).flashing("actionDone" -> "announcementAdded")
        }
        Redirect(routes.SosMessageApps.announcements(appId))
      }
    )
  }

  def removeAnnouncement(appId: String, announcementId: String) = Action { implicit request =>
    appsCollection.findOne(MongoDBObject("_id" -> new ObjectId(appId))).map { app =>
      val q = MongoDBObject("_id" -> new ObjectId(announcementId))
      val key = "apps." + app.get("name")
      val o = $unset(key)
      announcementsCollection.update(q, o, false, false)
      Redirect(routes.SosMessageApps.announcements(appId)).flashing("actionDone" -> "announcementRemoved")
    }
    Redirect(routes.SosMessageApps.announcements(appId))
  }

  def publishAnnouncement(appId: String, announcementId: String) = Action { implicit request =>
    val q = MongoDBObject("_id" -> new ObjectId(appId))
    appsCollection.findOne(q).map { app =>
      val key = "apps." + app.get("name") + ".published"
      val o = $set(key -> true, "modifiedAt" -> new Date())
      val q = MongoDBObject("_id" -> new ObjectId(announcementId))
      announcementsCollection.update(q, o, false, false)
    }
    Redirect(routes.SosMessageApps.announcements(appId)).flashing("actionDone" -> "announcementUpdated")
  }

  def unpublishAnnouncement(appId: String, announcementId: String) = Action { implicit request =>
    val q = MongoDBObject("_id" -> new ObjectId(appId))
    appsCollection.findOne(q).map { app =>
      val key = "apps." + app.get("name") + ".published"
      val o = $set(key -> false, "modifiedAt" -> new Date())
      val q = MongoDBObject("_id" -> new ObjectId(announcementId))
      announcementsCollection.update(q, o, false, false)
    }
    Redirect(routes.SosMessageApps.announcements(appId)).flashing("actionDone" -> "announcementUpdated")
  }
}
