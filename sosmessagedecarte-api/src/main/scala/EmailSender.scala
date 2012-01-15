package fr.arnk.sosmessage

import actors.Actor
import com.mongodb.DBObject
import javax.mail.internet.{ InternetAddress, MimeMessage }
import javax.mail.{ Message, Session }
import org.streum.configrity.Configuration

case class SendEmail(message: DBObject)

class EmailSender(config: Configuration) extends Actor {

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

  def act() {
    loop {
      react {
        case SendEmail(message) =>
          val tls = config[String]("mail.tls", "true")
          val host = config[String]("mail.host")
          val port = config[Int]("mail.port")
          val user = config[String]("mail.user")
          val password = config[String]("mail.password")

          val props = System.getProperties
          props.put("mail.smtp.auth", "true");
          props.put("mail.smtp.starttls.enable", tls);
          props.put("mail.smtp.host", host);
          props.put("mail.smtp.user", user);
          props.put("mail.smtp.password", password);
          props.put("mail.smtp.port", port.toString);
          val session = Session.getDefaultInstance(props)
          val mimeMessage = new MimeMessage(session)

          mimeMessage.setFrom(new InternetAddress(config[String]("mail.from")))
          mimeMessage.setRecipients(Message.RecipientType.TO, config[String]("mail.recipients"))
          mimeMessage.setSubject(Subject)
          val text = Text.format(message.get("category").toString, message.get("text").toString, message.get("contributorName").toString)
          mimeMessage.setText(text)

          val transport = session.getTransport("smtp");
          transport.connect(host, port, user, password);
          transport.sendMessage(mimeMessage, mimeMessage.getAllRecipients());
          transport.close();
      }
    }
  }
}
