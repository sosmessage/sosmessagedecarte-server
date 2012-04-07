package fr.arnk.sosmessage

import unfiltered.Cycle
import net.liftweb.json.JsonDSL._
import unfiltered.request._
import unfiltered.response.{ NoContent, BadRequest, Json, Ok }
import StandardConverters._

object SosMessageApiV1 {

  // Categories
  def publishedCategories: Cycle.Intent[Any, Any] = {
    case req @ GET(Path("/api/v1/categories")) =>
      val Params(form) = req
      val appName = form.get("appname") match {
        case Some(params) => Some(params(0))
        case None => None
      }

      val categories = SosMessage.publishedCategories(appName);
      val json = ("count", categories.size) ~ ("items", toJSON(categories))
      Ok ~> Json(json)
  }

  // Messages
  def randomMessage: Cycle.Intent[Any, Any] = {
    case req @ GET(Path(Seg("api" :: "v1" :: "categories" :: id :: "message" :: Nil))) =>
      val Params(form) = req
      val uid = form.get("uid") match {
        case Some(param) => Some(param(0))
        case None => None
      }
      SosMessage.randomMessage(id, uid) match {
        case None => NoContent
        case Some(message) => Ok ~> Json(toJSON(message))
      }
  }

  def messages: Cycle.Intent[Any, Any] = {
    case req @ GET(Path(Seg("api" :: "v1" :: "categories" :: id :: "messages" :: Nil))) =>
      val Params(form) = req
      val uid = form.get("uid") match {
        case Some(param) => Some(param(0))
        case None => None
      }

      val messages = SosMessage.messages(id, uid)
      val json = ("count", messages.size) ~ ("items", toJSON(messages))
      Ok ~> Json(json)
  }

  def bestMessages: Cycle.Intent[Any, Any] = {
    case req @ GET(Path(Seg("api" :: "v1" :: "categories" :: id :: "best" :: Nil))) =>
      val Params(form) = req
      val uid = form.get("uid") match {
        case Some(param) => Some(param(0))
        case None => None
      }
      val limit = form.get("limit") match {
        case Some(param) => Some(param(0).toInt)
        case None => None
      }

      val messages = SosMessage.bestMessages(id, uid, limit)
      val json = ("count", messages.size) ~ ("items", toJSON(messages))
      Ok ~> Json(json)
  }

  def worstMessages: Cycle.Intent[Any, Any] = {
    case req @ GET(Path(Seg("api" :: "v1" :: "categories" :: id :: "worst" :: Nil))) =>
      val Params(form) = req
      val uid = form.get("uid") match {
        case Some(param) => Some(param(0))
        case None => None
      }
      val limit = form.get("limit") match {
        case Some(param) => Some(param(0).toInt)
        case None => None
      }

      val messages = SosMessage.worstMessages(id, uid, limit)
      val json = ("count", messages.size) ~ ("items", toJSON(messages))
      Ok ~> Json(json)
  }

  def postMessage: Cycle.Intent[Any, Any] = {
    case req @ POST(Path(Seg("api" :: "v1" :: "categories" :: categoryId :: "message" :: Nil))) =>
      val Params(form) = req
      if (!form.contains("text") || !SosMessage.categoryExists(categoryId)) {
        BadRequest
      } else {
        val text = form.get("text").get(0)
        val contributorName = form.get("contributorName") match {
          case Some(param) => Some(param(0))
          case None => None
        }
        SosMessage.addMessage(categoryId, text, contributorName)
        NoContent
      }
  }

  def rateMessage: Cycle.Intent[Any, Any] = {
    case req @ POST(Path(Seg("api" :: "v1" :: "messages" :: messageId :: "rate" :: Nil))) =>
      val Params(form) = req
      if (!form.contains("uid") || !form.contains("rating")) {
        BadRequest
      } else {
        val uid = form("uid")(0)
        val rating = if (form("rating")(0).toInt > 5) 5 else form("rating")(0).toInt
        val message = SosMessage.rateMessage(messageId, uid, rating)
        Ok ~> Json(toJSON(message))
      }
  }

  def voteMessage: Cycle.Intent[Any, Any] = {
    case req @ POST(Path(Seg("api" :: "v1" :: "messages" :: messageId :: "vote" :: Nil))) =>
      val Params(form) = req
      if (!form.contains("uid") || !form.contains("vote")) {
        BadRequest
      } else {
        val uid = form("uid")(0)
        val vote = form("vote")(0).toInt
        if (vote != 1 && vote != -1) {
          BadRequest
        } else {
          val rating = if (vote == 1) 5 else 1
          val message = SosMessage.rateMessage(messageId, uid, rating)
          Ok ~> Json(toJSON(message))
        }
      }
  }

  // Comments
  def commentsForMessage: Cycle.Intent[Any, Any] = {
    case req @ GET(Path(Seg("api" :: "v1" :: "messages" :: messageId :: "comments" :: Nil))) =>
      val Params(form) = req
      val offset = form.get("offset") match {
        case Some(param) => Some(param(0).toInt)
        case None => None
      }
      val limit = form.get("limit") match {
        case Some(param) => Some(param(0).toInt)
        case None => None
      }

      val comments = SosMessage.comments(messageId, offset, limit)
      val json = ("count", comments.size) ~ ("items", toJSON(comments))
      Ok ~> Json(json)
  }

  def postComment: Cycle.Intent[Any, Any] = {
    case req @ POST(Path(Seg("api" :: "v1" :: "messages" :: messageId :: "comments" :: Nil))) =>
      val Params(form) = req
      if (!form.contains("uid") || !form.contains("text") || !SosMessage.messageExists(messageId)) {
        BadRequest
      } else {
        val uid = form.get("uid").get(0)
        val text = form.get("text").get(0)
        val author = form.get("author") match {
          case Some(param) => Some(param(0))
          case None => None
        }
        SosMessage.addComment(messageId, uid, text, author)
        NoContent
      }
  }

  // Announcements
  def publishedAnnouncements: Cycle.Intent[Any, Any] = {
    case req @ GET(Path("/api/v1/announcements")) =>
      val Params(form) = req
      val appName = form.get("appname") match {
        case Some(params) => Some(params(0))
        case None => None
      }

      val announcements = SosMessage.publishedAnnouncements(appName);
      val json = ("count", announcements.size) ~ ("items", toJSON(announcements))
      Ok ~> Json(json)
  }

  def toJSON[T](o: T)(implicit converter: JSONConverter[T]) =
    converter.toJSON(o)

  def toJSON[T](xs: Seq[T])(implicit converter: JSONConverter[T]) =
    xs.map(o => converter.toJSON(o))
}
