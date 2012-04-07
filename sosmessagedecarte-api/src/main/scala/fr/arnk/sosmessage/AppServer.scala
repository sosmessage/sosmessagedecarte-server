package fr.arnk.sosmessage

import unfiltered._

object AppServer {

  def main(args: Array[String]) {

    netty.Http(SosMessageConfig.get[Int]("server.port").getOrElse(3000))
      .handler(netty.cycle.Planify {
        SosMessageApi.publishedCategories orElse
          SosMessageApi.messages orElse SosMessageApi.randomMessage orElse
          SosMessageApi.bestMessages orElse SosMessageApi.worstMessages orElse
          SosMessageApi.postMessage orElse SosMessageApi.rateMessage orElse SosMessageApi.voteMessage orElse
          SosMessageApi.commentsForMessage orElse SosMessageApi.postComment orElse
          SosMessageApi.publishedAnnouncements orElse
          SosMessageApiV1.publishedCategories orElse
          SosMessageApiV1.messages orElse SosMessageApiV1.randomMessage orElse
          SosMessageApiV1.bestMessages orElse SosMessageApiV1.worstMessages orElse
          SosMessageApiV1.postMessage orElse SosMessageApiV1.rateMessage orElse SosMessageApiV1.voteMessage orElse
          SosMessageApiV1.commentsForMessage orElse SosMessageApiV1.postComment orElse
          SosMessageApiV1.publishedAnnouncements
      })
      .run()
  }

}
