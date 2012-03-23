package fr.arnk.sosmessage

import com.mongodb.casbah._
import java.util.Date

object TestDB {
  import SosMessageCollections._

  def initialize() {
    DB.drop(MessagesCollectionName)
    DB.drop(CategoriesCollectionName)
    DB.drop(CommentsCollectionName)
    DB.drop(AnnouncementsCollectionName)

    createCategories()
    createMessages()
    createAnnouncements()
  }

  def createCategories() {
    val smdcAppKey = "apps.smdc"
    val smdtAppKey = "apps.smdt"

    DB.collection(CategoriesCollectionName) {
      c =>
        val date = new Date()
        var builder = MongoDBObject.newBuilder
        builder += "name" -> "firstCategory"
        builder += "color" -> "#000"
        builder += "createdAt" -> date
        builder += "modifiedAt" -> date
        builder += "lastAddedMessageAt" -> date
        c += builder.result
        c.update(MongoDBObject("name" -> "firstCategory"), $set(smdcAppKey ->
          MongoDBObject("published" -> true, "order" -> 3), "modifiedAt" -> new Date()), false, false)

        builder = MongoDBObject.newBuilder
        builder += "name" -> "secondCategory"
        builder += "color" -> "#fff"
        builder += "createdAt" -> new Date(date.getTime + 10000)
        builder += "modifiedAt" -> new Date(date.getTime + 10000)
        builder += "lastAddedMessageAt" -> date
        c += builder.result
        c.update(MongoDBObject("name" -> "secondCategory"), $set(smdcAppKey ->
          MongoDBObject("published" -> true, "order" -> 2), "modifiedAt" -> new Date()), false, false)

        builder = MongoDBObject.newBuilder
        builder += "name" -> "thirdCategory"
        builder += "color" -> "#0f0"
        builder += "createdAt" -> new Date(date.getTime + 20000)
        builder += "modifiedAt" -> new Date(date.getTime + 20000)
        builder += "lastAddedMessageAt" -> date
        c += builder.result
        c.update(MongoDBObject("name" -> "thirdCategory"), $set(smdcAppKey ->
          MongoDBObject("published" -> false, "order" -> 1), "modifiedAt" -> new Date()), false, false)

        builder = MongoDBObject.newBuilder
        builder += "name" -> "fourthCategory"
        builder += "color" -> "#00f"
        builder += "createdAt" -> new Date()
        builder += "modifiedAt" -> new Date()
        builder += "lastAddedMessageAt" -> date
        c += builder.result
        c.update(MongoDBObject("name" -> "fourthCategory"), $set(smdcAppKey ->
          MongoDBObject("published" -> true, "order" -> 0), "modifiedAt" -> new Date()), false, false)
        c.update(MongoDBObject("name" -> "fourthCategory"), $set(smdtAppKey ->
          MongoDBObject("published" -> true, "order" -> 0), "modifiedAt" -> new Date()), false, false)

        builder = MongoDBObject.newBuilder
        builder += "name" -> "fifthCategory"
        builder += "color" -> "#0ff"
        builder += "createdAt" -> new Date()
        builder += "modifiedAt" -> new Date()
        builder += "lastAddedMessageAt" -> date
        c += builder.result
        c.update(MongoDBObject("name" -> "fifthCategory"), $set(smdtAppKey ->
          MongoDBObject("published" -> true, "order" -> 1), "modifiedAt" -> new Date()), false, false)
    }
  }

  def createMessages() {
    val (firstCategory, secondCategory, thirdCategory, fourthCategory) =
      DB.collection(CategoriesCollectionName) {
        c =>
          (
            c.findOne(MongoDBObject("name" -> "firstCategory")).get,
            c.findOne(MongoDBObject("name" -> "secondCategory")).get,
            c.findOne(MongoDBObject("name" -> "thirdCategory")).get,
            c.findOne(MongoDBObject("name" -> "fourthCategory")).get
          )
      }

    DB.collection(MessagesCollectionName) {
      c =>
        val date = new Date()
        var builder = MongoDBObject.newBuilder
        builder += "categoryId" -> firstCategory.get("_id")
        builder += "category" -> firstCategory.get("name")
        builder += "text" -> "First message in first category"
        builder += "contributorName" -> ""
        builder += "commentsCount" -> 0L
        builder += "state" -> "approved"
        builder += "state" -> "approved"
        builder += "createdAt" -> new Date(date.getTime + 10000)
        builder += "modifiedAt" -> new Date(date.getTime + 10000)
        builder += "random" -> scala.math.random
        c += builder.result

        builder = MongoDBObject.newBuilder
        builder += "categoryId" -> firstCategory.get("_id")
        builder += "category" -> firstCategory.get("name")
        builder += "text" -> "Second message in first category"
        builder += "contributorName" -> ""
        builder += "commentsCount" -> 0L
        builder += "state" -> "waiting"
        builder += "createdAt" -> new Date(date.getTime + 15000)
        builder += "modifiedAt" -> new Date(date.getTime + 15000)
        builder += "random" -> scala.math.random
        c += builder.result

        builder = MongoDBObject.newBuilder
        builder += "categoryId" -> firstCategory.get("_id")
        builder += "category" -> firstCategory.get("name")
        builder += "text" -> "Third message in first category"
        builder += "contributorName" -> ""
        builder += "commentsCount" -> 0L
        builder += "state" -> "approved"
        builder += "createdAt" -> new Date(date.getTime + 20000)
        builder += "modifiedAt" -> new Date(date.getTime + 20000)
        builder += "random" -> scala.math.random
        c += builder.result

        builder = MongoDBObject.newBuilder
        builder += "categoryId" -> secondCategory.get("_id")
        builder += "category" -> secondCategory.get("name")
        builder += "text" -> "First message in second category"
        builder += "contributorName" -> ""
        builder += "commentsCount" -> 0L
        builder += "state" -> "approved"
        builder += "createdAt" -> new Date(date.getTime + 20000)
        builder += "modifiedAt" -> new Date(date.getTime + 20000)
        builder += "random" -> scala.math.random
        c += builder.result

        builder = MongoDBObject.newBuilder
        builder += "categoryId" -> secondCategory.get("_id")
        builder += "category" -> secondCategory.get("name")
        builder += "text" -> "Second message in second category"
        builder += "contributorName" -> ""
        builder += "commentsCount" -> 0L
        builder += "state" -> "approved"
        builder += "createdAt" -> new Date(date.getTime + 20000)
        builder += "modifiedAt" -> new Date(date.getTime + 20000)
        builder += "random" -> scala.math.random
        c += builder.result

        builder = MongoDBObject.newBuilder
        builder += "categoryId" -> thirdCategory.get("_id")
        builder += "category" -> thirdCategory.get("name")
        builder += "text" -> "First message in third category"
        builder += "contributorName" -> ""
        builder += "commentsCount" -> 0L
        builder += "state" -> "approved"
        builder += "createdAt" -> new Date(date.getTime + 20000)
        builder += "modifiedAt" -> new Date(date.getTime + 20000)
        builder += "random" -> scala.math.random
        c += builder.result

        builder = MongoDBObject.newBuilder
        builder += "categoryId" -> fourthCategory.get("_id")
        builder += "category" -> fourthCategory.get("name")
        builder += "text" -> "First message in fourth category"
        builder += "contributorName" -> ""
        builder += "commentsCount" -> 0L
        builder += "state" -> "approved"
        builder += "createdAt" -> new Date(date.getTime + 30000)
        builder += "modifiedAt" -> new Date(date.getTime + 30000)
        builder += "random" -> scala.math.random
        c += builder.result
    }
  }

  def createAnnouncements() {
    val smdcAppKey = "apps.smdc"
    val smdtAppKey = "apps.smdt"

    DB.collection(AnnouncementsCollectionName) {
      c =>
        val date = new Date()
        var builder = MongoDBObject.newBuilder
        builder += "title" -> "First announcement"
        builder += "text" -> "Text of first announcement"
        builder += "url" -> "http://first/announcement"
        builder += "buttons" -> MongoDBObject("validate" -> "First validate", "cancel" -> "First cancel")
        builder += "createdAt" -> new Date(date.getTime + 10000)
        builder += "modifiedAt" -> new Date(date.getTime + 10000)
        c += builder.result
        c.update(MongoDBObject("title" -> "First announcement"), $set(smdcAppKey ->
          MongoDBObject("published" -> true), "modifiedAt" -> new Date()), false, false)

        builder = MongoDBObject.newBuilder
        builder += "title" -> "Second announcement"
        builder += "text" -> "Text of second announcement"
        builder += "url" -> "http://second/announcement"
        builder += "buttons" -> MongoDBObject("validate" -> "Second validate", "cancel" -> "Second cancel")
        builder += "createdAt" -> new Date(date.getTime + 15000)
        builder += "modifiedAt" -> new Date(date.getTime + 15000)
        c += builder.result
        c.update(MongoDBObject("title" -> "Second announcement"), $set(smdcAppKey ->
          MongoDBObject("published" -> true), "modifiedAt" -> new Date()), false, false)

        builder = MongoDBObject.newBuilder
        builder += "title" -> "Third announcement"
        builder += "text" -> "Text of third announcement"
        builder += "url" -> "http://third/announcement"
        builder += "buttons" -> MongoDBObject("validate" -> "Third validate", "cancel" -> "Third cancel")
        builder += "createdAt" -> new Date(date.getTime + 20000)
        builder += "modifiedAt" -> new Date(date.getTime + 20000)
        c += builder.result
        c.update(MongoDBObject("title" -> "Third announcement"), $set(smdtAppKey ->
          MongoDBObject("published" -> false), "modifiedAt" -> new Date()), false, false)

        builder = MongoDBObject.newBuilder
        builder += "title" -> "Fourth announcement"
        builder += "text" -> "Text of fourth announcement"
        builder += "url" -> "http://fourth/announcement"
        builder += "buttons" -> MongoDBObject("validate" -> "Fourth validate", "cancel" -> "Fourth cancel")
        builder += "createdAt" -> new Date(date.getTime + 25000)
        builder += "modifiedAt" -> new Date(date.getTime + 25000)
        c += builder.result
        c.update(MongoDBObject("title" -> "Fourth announcement"), $set(smdtAppKey ->
          MongoDBObject("published" -> true), "modifiedAt" -> new Date()), false, false)

        builder = MongoDBObject.newBuilder
        builder += "title" -> "Fifth announcement"
        builder += "text" -> "Text of fifth announcement"
        builder += "url" -> "http://fifth/announcement"
        builder += "buttons" -> MongoDBObject("validate" -> "Fifth validate", "cancel" -> "Fifth cancel")
        builder += "createdAt" -> new Date(date.getTime + 35000)
        builder += "modifiedAt" -> new Date(date.getTime + 35000)
        c += builder.result
        c.update(MongoDBObject("title" -> "Fifth announcement"), $set(smdcAppKey ->
          MongoDBObject("published" -> false), "modifiedAt" -> new Date()), false, false)
    }
  }

}