/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package services

import java.net.URLEncoder._

import play.api._
import play.api.libs.ws._
import play.api.Play.current
import models.HubUtils

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
    val props = Map("to" -> munge(to),
                    "from" -> HubUtils.replyEmail,
                    "subject" -> subject,
                    "text" -> msg)
    send(props)
  }

  def feedback(from: String, msg: String, reply: Boolean) = {
    val props = Map("to" -> munge(adminEmail),
                    "from" -> from,
                    "subject" -> s"${HubUtils.siteName} Feedback",
                    "text" -> msg)
    send(if (reply) props + ("h:Reply-To" -> from) else props)
  }

  def subscriberEmails(to: String, subject: String, msg: String) = {
    val props = Map("to" -> munge(to),
                    "from" -> HubUtils.replyEmail,
                    "sender" -> HubUtils.replyEmail,
                    "subject" -> subject,
                    "text" -> msg)
    send(props)
  }

  def send(props: Map[String, String])

  def munge(addrs: String) = if (play.api.Play.isTest(play.api.Play.current)) mailinate(addrs) else addrs
  def mailinate(addrs: String) = addrs.split(",").toList.map(_.replace("@", "at") + "@mailinator.com").mkString(",")
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
