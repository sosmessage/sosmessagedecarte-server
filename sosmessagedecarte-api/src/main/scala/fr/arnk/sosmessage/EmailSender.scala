package fr.arnk.sosmessage

import akka.actor._
import com.mongodb.DBObject
import javax.mail.internet.{ InternetAddress, MimeMessage }
import javax.mail.{ Message => JMessage, Session }
import org.streum.configrity.Configuration

case class SendEmail(message: DBObject)

object EmailSender {

  private val system = ActorSystem("EmaiSenderSystem")
  private val emailSender = system.actorOf(Props(new EmailSender), name = "emailSender")

  def get = {
    emailSender
  }

  def stop = {
    system.stop(emailSender)
  }

}

class EmailSender extends Actor {

  private val Subject = "[Moderation] New message waiting for approval"

  private val Text = """
    Hi,

    There is a new message waiting your approval!

    Category:
      %s

    Message:
      %s

    Contributed by %s
  """

  def receive = {
    case SendEmail(message) =>
      val auth = SosMessageConfig[String]("mail.auth").getOrElse("false")
      val tls = SosMessageConfig[String]("mail.tls").getOrElse("true")
      val host = SosMessageConfig[String]("mail.host").get
      val port = SosMessageConfig[Int]("mail.port").get
      val user = SosMessageConfig[String]("mail.user").get
      val password = SosMessageConfig[String]("mail.password").get

      val props = System.getProperties
      if (auth == "true") {
        props.put("mail.smtp.auth", "true");
        props.put("mail.smtp.user", user);
        props.put("mail.smtp.password", password);
      } else {
        props.put("mail.smtp.auth", "false");
      }
      props.put("mail.smtp.starttls.enable", tls);
      props.put("mail.smtp.host", host);
      props.put("mail.smtp.port", port.toString);
      val session = Session.getDefaultInstance(props)
      val mimeMessage = new MimeMessage(session)

      mimeMessage.setFrom(new InternetAddress(SosMessageConfig[String]("mail.from").get))
      mimeMessage.setRecipients(JMessage.RecipientType.TO, SosMessageConfig[String]("mail.recipients").get)
      mimeMessage.setSubject(Subject)
      val text = Text.format(message.get("category").toString, message.get("text").toString, message.get("contributorName").toString)
      mimeMessage.setText(text)

      val transport = session.getTransport("smtp");
      transport.connect(host, port, user, password);
      transport.sendMessage(mimeMessage, mimeMessage.getAllRecipients());
      transport.close();
  }
}
