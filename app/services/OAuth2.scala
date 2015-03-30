package services

import play.api.Application
import play.api.Play
import play.api.http.{MimeTypes, HeaderNames}
import play.api.libs.ws.WS
import play.api.libs.ws._
import play.api.mvc.{Results, Action, Controller}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class OAuth2(application: Application) {
  lazy val mitAuthId = application.configuration.getString("mit.client.id").get
  lazy val mitAuthSecret = application.configuration.getString("mit.client.secret").get

  def getAuthorizationUrl(redirectUri: String, scope: String, state: String): String = {
    val baseUrl = application.configuration.getString("mit.redirect.url").get
    baseUrl.format(mitAuthId, redirectUri, scope, state)
  }

  def getToken(code: String): Future[String] = {
    val tokenResponse = WS.url("https://oidc.mit.edu/token")(application).
      withQueryString("client_id" -> mitAuthId,
        "client_secret" -> mitAuthSecret,
        "code" -> code,
        "grant_type" -> "authorization_code",
        "redirect_uri" -> "http://localhost:9000/_oauth-callback").
      withAuth(mitAuthId, mitAuthSecret, WSAuthScheme.BASIC).
      withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).
      post(Results.EmptyContent())

    tokenResponse.flatMap { response =>
      (response.json \ "access_token").asOpt[String].fold(Future.failed[String](new IllegalStateException("Sod off!"))) { accessToken =>
        Future.successful(accessToken)
      }
    }
  }
}

object OAuth2 extends Controller {
  lazy val oauth2 = new OAuth2(Play.current)

  def callback(codeOpt: Option[String] = None, stateOpt: Option[String] = None) = Action.async { implicit request =>
    println(codeOpt)
    println(stateOpt)
    println(request.session.get("oauth-state"))
    println(request)
    (for {
      code <- codeOpt
      state <- stateOpt
      oauthState <- request.session.get("oauth-state")
    } yield {
      if (state == oauthState) {
        oauth2.getToken(code).map { accessToken =>
          Redirect(services.routes.OAuth2.success()).withSession("oauth-token" -> accessToken)
        }.recover {
          case ex: IllegalStateException => Unauthorized(ex.getMessage)
        }
      }
      else {
        Future.successful(BadRequest("Invalid mit login"))
      }
    }).getOrElse(Future.successful(BadRequest("No parameters supplied")))
  }

  def success() = Action.async { request =>
    println("___oath token start___")
    println(request.session.get("oauth-token"))
    println("___oath token end___")
    implicit val app = Play.current
    request.session.get("oauth-token").fold(Future.successful(Unauthorized("No way Jose"))) { authToken =>
      WS.url("https://oidc.mit.edu/userinfo").
        withHeaders(HeaderNames.AUTHORIZATION -> ("Bearer " + s"$authToken")).
        get().map { response =>
          Ok(response.json)
        }
    }
  }
}
