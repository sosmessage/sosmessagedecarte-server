package fr.arnk.sosmessage

import org.specs._
import unfiltered._

class SosMessageSpec extends Specification with unfiltered.spec.netty.Served {

  // Initialize test config
  SosMessageConfig.set("database.host", "127.0.0.1")
  SosMessageConfig.set("database.port", 27017)
  SosMessageConfig.set("database.name", "sosmessagedetest")
  SosMessageConfig.set("server.port", 3000)

  def setup = {
    _.handler(netty.cycle.Planify {
      SosMessageApi.publishedCategories orElse
        SosMessageApi.messages orElse SosMessageApi.randomMessage orElse
        SosMessageApi.bestMessages orElse SosMessageApi.worstMessages orElse
        SosMessageApi.postMessage orElse SosMessageApi.rateMessage orElse SosMessageApi.voteMessage orElse
        SosMessageApi.commentsForMessage orElse SosMessageApi.postComment orElse
        SosMessageApi.publishedAnnouncements
    })
  }
}
