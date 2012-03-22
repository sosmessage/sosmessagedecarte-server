package fr.arnk.sosmessage

import org.slf4j.LoggerFactory
import unfiltered.Cycle
import unfiltered.request.Path
import java.text.SimpleDateFormat
import java.util.{ Locale, Date }
import unfiltered.response.Pass

trait Logged {

  def log = LoggerFactory.getLogger(this.getClass.getName);

}

object RequestLog {

  val logger = LoggerFactory.getLogger(RequestLog.getClass);
  val DateFmt = "EEE, d MMM yyyy HH:mm:ss Z"

  def logRequest: Cycle.Intent[Any, Any] = {
    case r @ Path(path) =>
      logger.info("%s %s %s" format (
        new SimpleDateFormat(DateFmt, Locale.US).format(
          new Date()), r.method, path)
      )
      Pass
  }

}
