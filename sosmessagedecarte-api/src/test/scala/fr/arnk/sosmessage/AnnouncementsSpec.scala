package fr.arnk.sosmessage

import org.specs._
import unfiltered._
import org.streum.configrity.Configuration
import com.mongodb.casbah._
import net.liftweb.json._
import java.util.Date
import com.mongodb.{ BasicDBObject, DBObject }

object AnnouncementsSpec extends SosMessageSpec {

  import SosMessageCollections._

  "The announcements API v2" should {
    doBefore {
      TestDB.initialize
    }

    "retrieve only published announcements for given app" in {
      val resp = http(host / "api" / "v2" / "announcements" <<? Map("appname" -> "smdt") as_str)
      val json = parse(resp)

      json \ "meta" \ "code" must_== JInt(200)

      val response = json \ "response"
      response \ "count" must_== JInt(1)

      val JArray(items) = response \ "items"
      items.size must_== 1

      val firstItem = items(0)
      firstItem \ "title" must_== JString("Fourth announcement")
      firstItem \ "text" must_== JString("Text of fourth announcement")
      firstItem \ "url" must_== JString("http://fourth/announcement")
    }
  }

}