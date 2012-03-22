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
          SosMessageApi.publishedAnnouncements
      })
      .run()
  }

}
