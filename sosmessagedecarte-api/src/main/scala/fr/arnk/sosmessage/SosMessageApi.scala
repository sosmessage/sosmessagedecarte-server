package fr.arnk.sosmessage

import unfiltered.Cycle
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import unfiltered.request._
import unfiltered.response.{ NoContent, BadRequest, Json, Ok, InternalServerError }
import StandardConverters._
import unfiltered._

object SosMessageApi {

  // Categories
  def publishedCategories: Cycle.Intent[Any, Any] = {
    case req @ GET(Path("/api/v2/categories")) =>
      val Params(form) = req
      val appName = form.get("appname") match {
        case Some(params) => Some(params(0))
        case None => None
      }

      try {
        val categories = SosMessage.publishedCategories(appName);
        val json = ("meta", ("code", 200)) ~
          ("response", (("count", categories.size) ~ ("items", toJSON(categories))))
        Ok ~> Json(json)
      } catch {
        case e: Exception => {
          val json = ("meta", ("code", 500) ~ ("errorType", "ServerError")) ~
            ("errorDetails", e.getMessage) ~ ("response", JObject(List()))
          InternalServerError ~> Json(json)
        }
      }

  }

  // Messages
  def randomMessage: Cycle.Intent[Any, Any] = {
    case req @ GET(Path(Seg("api" :: "v2" :: "categories" :: id :: "message" :: Nil))) =>
      val Params(form) = req
      val uid = form.get("uid") match {
        case Some(param) => Some(param(0))
        case None => None
      }

      if (SosMessage.categoryExists(id)) {
        try {
          SosMessage.randomMessage(id, uid) match {
            case None => NoContent
            case Some(message) => {
              val json = ("meta", ("code", 200)) ~
                ("response", toJSON(message))
              Ok ~> Json(json)
            }
          }
        } catch {
          case e: Exception => {
            val json = ("meta", ("code", 500) ~ ("errorType", "ServerError")) ~
              ("errorDetails", e.getMessage) ~ ("response", JObject(List()))
            InternalServerError ~> Json(json)
          }
        }
      } else {
        val json = ("meta", ("code", 400) ~ ("errorType", "UnknownCategory")) ~
          ("errorDetails", "The category does not exist.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      }
  }

  def messages: Cycle.Intent[Any, Any] = {
    case req @ GET(Path(Seg("api" :: "v2" :: "categories" :: id :: "messages" :: Nil))) =>
      val Params(form) = req
      val uid = form.get("uid") match {
        case Some(param) => Some(param(0))
        case None => None
      }

      if (SosMessage.categoryExists(id)) {
        try {
          val messages = SosMessage.messages(id, uid)
          val json = ("meta", ("code", 200)) ~
            ("response", ("count", messages.size) ~ ("items", toJSON(messages)))
          Ok ~> Json(json)
        } catch {
          case e: Exception => {
            val json = ("meta", ("code", 500) ~ ("errorType", "ServerError")) ~
              ("errorDetails", e.getMessage) ~ ("response", JObject(List()))
            InternalServerError ~> Json(json)
          }
        }
      } else {
        val json = ("meta", ("code", 400) ~ ("errorType", "UnknownCategory")) ~
          ("errorDetails", "The category does not exist.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      }
  }

  def bestMessages: Cycle.Intent[Any, Any] = {
    case req @ GET(Path(Seg("api" :: "v2" :: "categories" :: id :: "best" :: Nil))) =>
      val Params(form) = req
      val uid = form.get("uid") match {
        case Some(param) => Some(param(0))
        case None => None
      }
      val limit = form.get("limit") match {
        case Some(param) => Some(param(0).toInt)
        case None => None
      }

      if (SosMessage.categoryExists(id)) {
        try {
          val messages = SosMessage.bestMessages(id, uid, limit)
          val json = ("meta", ("code", 200)) ~
            ("response", ("count", messages.size) ~ ("items", toJSON(messages)))
          Ok ~> Json(json)
        } catch {
          case e: Exception => {
            val json = ("meta", ("code", 500) ~ ("errorType", "ServerError")) ~
              ("errorDetails", e.getMessage) ~ ("response", JObject(List()))
            InternalServerError ~> Json(json)
          }
        }
      } else {
        val json = ("meta", ("code", 400) ~ ("errorType", "UnknownCategory")) ~
          ("errorDetails", "The category does not exist.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      }
  }

  def worstMessages: Cycle.Intent[Any, Any] = {
    case req @ GET(Path(Seg("api" :: "v2" :: "categories" :: id :: "worst" :: Nil))) =>
      val Params(form) = req
      val uid = form.get("uid") match {
        case Some(param) => Some(param(0))
        case None => None
      }
      val limit = form.get("limit") match {
        case Some(param) => Some(param(0).toInt)
        case None => None
      }

      if (SosMessage.categoryExists(id)) {
        try {
          val messages = SosMessage.worstMessages(id, uid, limit)
          val json = ("meta", ("code", 200)) ~
            ("response", ("count", messages.size) ~ ("items", toJSON(messages)))
          Ok ~> Json(json)
        } catch {
          case e: Exception => {
            val json = ("meta", ("code", 500) ~ ("errorType", "ServerError")) ~
              ("errorDetails", e.getMessage) ~ ("response", JObject(List()))
            InternalServerError ~> Json(json)
          }
        }
      } else {
        val json = ("meta", ("code", 400) ~ ("errorType", "UnknownCategory")) ~
          ("errorDetails", "The category does not exist.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      }
  }

  def postMessage: Cycle.Intent[Any, Any] = {
    case req @ POST(Path(Seg("api" :: "v2" :: "categories" :: categoryId :: "message" :: Nil))) =>
      val Params(form) = req
      if (!form.contains("text")) {
        val json = ("meta", ("code", 400) ~ ("errorType", "MissingParameter")) ~
          ("errorDetails", "The 'text' parameter is required.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      } else if (!SosMessage.categoryExists(categoryId)) {
        val json = ("meta", ("code", 400) ~ ("errorType", "UnknownCategory")) ~
          ("errorDetails", "The category does not exist.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      } else {
        try {
          val text = form.get("text").get(0)
          val contributorName = form.get("contributorName") match {
            case Some(param) => Some(param(0))
            case None => None
          }
          SosMessage.addMessage(categoryId, text, contributorName)
          val json = ("meta", ("code", 200)) ~
            ("response", JObject(List()))
          Ok ~> Json(json)
        } catch {
          case e: Exception => {
            val json = ("meta", ("code", 500) ~ ("errorType", "ServerError")) ~
              ("errorDetails", e.getMessage) ~ ("response", JObject(List()))
            InternalServerError ~> Json(json)
          }
        }
      }
  }

  def rateMessage: Cycle.Intent[Any, Any] = {
    case req @ POST(Path(Seg("api" :: "v2" :: "messages" :: messageId :: "rate" :: Nil))) =>
      val Params(form) = req
      if (!form.contains("uid")) {
        val json = ("meta", ("code", 400) ~ ("errorType", "MissingParameter")) ~
          ("errorDetails", "The 'uid' parameter is required.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      } else if (!form.contains("rating")) {
        val json = ("meta", ("code", 400) ~ ("errorType", "MissingParameter")) ~
          ("errorDetails", "The 'rating' parameter is required.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      } else {
        try {
          val uid = form("uid")(0)
          val rating = if (form("rating")(0).toInt > 5) 5 else form("rating")(0).toInt
          val message = SosMessage.rateMessage(messageId, uid, rating)
          val json = ("meta", ("code", 200)) ~
            ("response", toJSON(message))
          Ok ~> Json(json)
        } catch {
          case e: Exception => {
            val json = ("meta", ("code", 500) ~ ("errorType", "ServerError")) ~
              ("errorDetails", e.getMessage) ~ ("response", JObject(List()))
            InternalServerError ~> Json(json)
          }
        }
      }
  }

  def voteMessage: Cycle.Intent[Any, Any] = {
    case req @ POST(Path(Seg("api" :: "v2" :: "messages" :: messageId :: "vote" :: Nil))) =>
      val Params(form) = req
      if (!form.contains("uid")) {
        val json = ("meta", ("code", 400) ~ ("errorType", "MissingParameter")) ~
          ("errorDetails", "The 'uid' parameter is required.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      } else if (!form.contains("vote")) {
        val json = ("meta", ("code", 400) ~ ("errorType", "MissingParameter")) ~
          ("errorDetails", "The 'vote' parameter is required.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      } else {
        try {
          val uid = form("uid")(0)
          val vote = form("vote")(0).toInt
          if (vote != 1 && vote != -1) {
            val json = ("meta", ("code", 400) ~ ("errorType", "WrongParameter")) ~
              ("errorDetails", "The 'vote' parameter must be -1 or 1.") ~
              ("response", JObject(List()))
            BadRequest ~> Json(json)
          } else {
            val rating = if (vote == 1) 5 else 1
            val message = SosMessage.rateMessage(messageId, uid, rating)
            val json = ("meta", ("code", 200)) ~
              ("response", toJSON(message))
            Ok ~> Json(json)
          }
        } catch {
          case e: Exception => {
            val json = ("meta", ("code", 500) ~ ("errorType", "ServerError")) ~
              ("errorDetails", e.getMessage) ~ ("response", JObject(List()))
            InternalServerError ~> Json(json)
          }
        }

      }
  }

  // Comments
  def commentsForMessage: Cycle.Intent[Any, Any] = {
    case req @ GET(Path(Seg("api" :: "v2" :: "messages" :: messageId :: "comments" :: Nil))) =>
      val Params(form) = req
      val offset = form.get("offset") match {
        case Some(param) => Some(param(0).toInt)
        case None => None
      }
      val limit = form.get("limit") match {
        case Some(param) => Some(param(0).toInt)
        case None => None
      }

      try {
        val comments = SosMessage.comments(messageId, offset, limit)
        val json = ("meta", ("code", 200)) ~
          ("response", ("count", comments.size) ~ ("items", toJSON(comments)))
        Ok ~> Json(json)
      } catch {
        case e: Exception => {
          val json = ("meta", ("code", 500) ~ ("errorType", "ServerError")) ~
            ("errorDetails", e.getMessage) ~ ("response", JObject(List()))
          InternalServerError ~> Json(json)
        }
      }
  }

  def postComment: Cycle.Intent[Any, Any] = {
    case req @ POST(Path(Seg("api" :: "v2" :: "messages" :: messageId :: "comments" :: Nil))) =>
      val Params(form) = req
      if (!form.contains("uid")) {
        val json = ("meta", ("code", 400) ~ ("errorType", "MissingParameter")) ~
          ("errorDetails", "The 'uid' parameter is required.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      } else if (!form.contains("text")) {
        val json = ("meta", ("code", 400) ~ ("errorType", "MissingParameter")) ~
          ("errorDetails", "The 'text' parameter is required.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      } else if (!SosMessage.messageExists(messageId)) {
        val json = ("meta", ("code", 400) ~ ("errorType", "UnknownCategory")) ~
          ("errorDetails", "The category does not exist.") ~
          ("response", JObject(List()))
        BadRequest ~> Json(json)
      } else {
        try {
          val uid = form.get("uid").get(0)
          val text = form.get("text").get(0)
          val author = form.get("author") match {
            case Some(param) => Some(param(0))
            case None => None
          }
          SosMessage.addComment(messageId, uid, text, author)
          val json = ("meta", ("code", 200)) ~
            ("response", JObject(List()))
          Ok ~> Json(json)
        } catch {
          case e: Exception => {
            val json = ("meta", ("code", 500) ~ ("errorType", "ServerError")) ~
              ("errorDetails", e.getMessage) ~ ("response", JObject(List()))
            InternalServerError ~> Json(json)
          }
        }
      }
  }

  // Announcements
  def publishedAnnouncements: Cycle.Intent[Any, Any] = {
    case req @ GET(Path("/api/v2/announcements")) =>
      val Params(form) = req
      val appName = form.get("appname") match {
        case Some(params) => Some(params(0))
        case None => None
      }

      try {
        val announcements = SosMessage.publishedAnnouncements(appName);
        val json = ("meta", ("code", 200)) ~
          ("response", ("count", announcements.size) ~ ("items", toJSON(announcements)))
        Ok ~> Json(json)
      } catch {
        case e: Exception => {
          val json = ("meta", ("code", 500) ~ ("errorType", "ServerError")) ~
            ("errorDetails", e.getMessage) ~ ("response", JObject(List()))
          InternalServerError ~> Json(json)
        }
      }
  }
}
