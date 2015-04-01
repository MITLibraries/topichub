package controllers

import scala.concurrent.Future
import play.api._
import play.api.Play.current
import play.api.mvc._
import org.pac4j.play._
import org.pac4j.core.client._
import org.pac4j.core.context._
import org.pac4j.oauth.client._
import play.api.mvc.Results._

object Global extends GlobalSettings {

  override def onError(request: RequestHeader, t: Throwable) = {
    Future.successful(InternalServerError(
      views.html.error500.render()
    ))
  }

  override def onStart(app: Application) {
    BaseConfig.setErrorPage401(views.html.error401.render().toString())
    BaseConfig.setErrorPage403(views.html.error403.render().toString())

    val baseUrl = Play.application.configuration.getString("baseUrl").get

    // OAuth
    val githubClient = new GitHubClient(
        Play.application.configuration.getString("github.key").get,
        Play.application.configuration.getString("github.secret").get)

    println("______________App Just Started________________")

    val clients = new Clients(baseUrl + "/callback", githubClient)
    Config.setClients(clients)
    // for test purposes : profile timeout = 60 seconds
    // Config.setProfileTimeout(60)
  }
}
