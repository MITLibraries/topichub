/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package services

import java.net.URLEncoder._

import play.api._
import play.api.libs.ws._
import play.api.Play.current

/** Service for email delivery
  * TODO: refactor for real plug-in modularity, this contains
  *  lot of Mailgun-specific logic that is not isolated
  *
  * @author richardrodgers
  */

object Emailer extends EmailService {
  // only implementation thus far is Mailgun - so just wired in
  override def send(props: Map[String, String]) = {
    MailgunProvider.send(props)
  }
}

trait EmailService {

  val adminEmail = Play.configuration.getString("hub.admin.email").get

  def notify(to: String, subject: String, msg: String) {
    val props = Map("to" -> to, "from" -> "noreply@scoap3hub.org",
                    "subject" -> subject, "text" -> msg)
    send(props)
  }

  def feedback(from: String, msg: String, reply: Boolean) = {
    val props = Map("to" -> adminEmail, "from" -> from,
                    "subject" -> "SCOAP3Hub Feedback", "text" -> msg)
    send(if (reply) props + ("h:Reply-To" -> from) else props)
  }

  def subscriberEmails(to: String, subject: String, msg: String) = {
    val props = Map("to" -> to, "from" -> "noreply@scoap3hub.org",
                    "sender" -> "noreply@scoap3hub.org",
                    "subject" -> subject,
                    "text" -> msg)
    send(props)
  }

  def send(props: Map[String, String])
}

object MailgunProvider extends EmailService {

  lazy val svcUrl = Play.configuration.getString("hub.email.url").get
  lazy val apiKey = Play.configuration.getString("hub.email.apikey").get

  override def send(props: Map[String, String]) = {
    val params = props.toList.map({ case (key, value) => key + "=" + encode(value, "UTF-8")}).mkString("&")
    WS.url(svcUrl).withHeaders("Content-Type" -> "application/x-www-form-urlencoded").
    withAuth("api", apiKey, WSAuthScheme.BASIC).post(params)
  }
}
