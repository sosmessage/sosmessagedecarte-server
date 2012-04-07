package fr.arnk.sosmessage.v1

import fr.arnk.sosmessage._

import org.specs._
import unfiltered._
import org.streum.configrity.Configuration
import com.mongodb.casbah._
import net.liftweb.json._
import java.util.Date
import com.mongodb.{ BasicDBObject, DBObject }

object CategoriesSpec extends SosMessageSpec {

  import SosMessageCollections._

  "The categories API v1" should {
    doBefore {
      TestDB.initialize
    }

    "retrieve ordered published categories" in {
      val resp = http(host / "api" / "v1" / "categories" as_str)
      val json = parse(resp)

      json \ "count" must_== JInt(3)

      val JArray(items) = json \ "items"
      items.size must_== 3

      val firstItem = items(0)
      firstItem \ "name" must_== JString("firstCategory")
      firstItem \ "color" must_== JString("#000")

      val secondItem = items(1)
      secondItem \ "name" must_== JString("secondCategory")
      secondItem \ "color" must_== JString("#fff")

      val thirdItem = items(2)
      thirdItem \ "name" must_== JString("fourthCategory")
      thirdItem \ "color" must_== JString("#00f")
    }

    "retrieve ordered published categories for the smdt appname" in {
      val resp = http(host / "api" / "v1" / "categories" <<? Map("appname" -> "smdt") as_str)
      val json = parse(resp)

      json \ "count" must_== JInt(2)

      val JArray(items) = json \ "items"
      items.size must_== 2

      val firstItem = items(0)
      firstItem \ "name" must_== JString("fifthCategory")
      firstItem \ "color" must_== JString("#0ff")

      val secondItem = items(1)
      secondItem \ "name" must_== JString("fourthCategory")
      secondItem \ "color" must_== JString("#00f")

    }
  }

}