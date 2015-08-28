package services

import play.api.Application
import play.api.Play
import play.api.http.{MimeTypes, HeaderNames}
import play.api.libs.ws.WS
import play.api.libs.ws._
import play.api.mvc.{Results, Action, Controller}
import models._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._

class OAuth2(application: Application) {
  lazy val auth_id = application.configuration.getString("auth.client.id").get
  lazy val auth_secret = application.configuration.getString("auth.client.secret").get
  lazy val auth_callback_url = application.configuration.getString("auth.client.callback_url").get
  lazy val auth_name_field = application.configuration.getString("auth.response.name_field").get
  lazy val auth_sub_field = application.configuration.getString("auth.response.sub_field").get
  lazy val auth_email_field = application.configuration.getString("auth.response.email_field").get
  lazy val auth_token_url = application.configuration.getString("auth.client.token_url").get
  lazy val auth_profile_url = application.configuration.getString("auth.client.profile_url").get
  lazy val auth_identity_provider = application.configuration.getString("auth.reponse.identity_provider").get

  def getAuthorizationUrl(redirectUri: String, scope: String, state: String): String = {
    val baseUrl = application.configuration.getString("auth.client.external_auth_url").get
    baseUrl.format(auth_id, redirectUri, scope, state)
  }

  def getToken(code: String): Future[String] = {
    val data = Map(
      "client_id" -> Seq(auth_id),
      "client_secret" -> Seq(auth_secret),
      "code" -> Seq(code),
      "grant_type" -> Seq("authorization_code"),
      "redirect_uri" -> Seq(auth_callback_url)
      )

    val tokenResponse = WS.url(auth_token_url)(application).
      withAuth(auth_id, auth_secret, WSAuthScheme.BASIC).
      withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).
      post(data)

    tokenResponse.flatMap { response =>
      (response.json \ "access_token").asOpt[String].fold(Future.failed[String](new IllegalStateException("""
        Authenication Resulted in an Illegal State. Please quit your browser and try again.
        If it is not resolved, please <a href="/contact">report the problem</a>."""))) { accessToken =>
        Future.successful(accessToken)
      }
    }
  }
}

object OAuth2 extends Controller {
  lazy val oauth2 = new OAuth2(Play.current)

  def redirectToExternalAuthServer(requestUri: String) = Action { implicit request =>
    if ( play.api.Play.isTest(play.api.Play.current) ) {
      val response = Json.parse("""
          { "id":"current_user", "sub":"current_user", "name":"Firstname M Lastname", "preferred_username":"flastname",
           "given_name":"Firstname", "family_name":"Lastname", "middle_name":"M",
           "email":"flastname@example.com", "email_verified":true }
         """)
      lookupUser(response)
    } else {
      Redirect(requestUri)
    }
  }

  def callback(codeOpt: Option[String] = None, stateOpt: Option[String] = None) = Action.async { implicit request =>
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
    implicit val app = Play.current
    request.session.get("oauth-token").fold(Future.successful(Unauthorized("Authorization Failed."))) { authToken =>
      WS.url(oauth2.auth_profile_url).
        withHeaders(HeaderNames.AUTHORIZATION -> ("Bearer " + s"$authToken")).
        get().map { response =>
          lookupUser(response.json)
        }
    }
  }

  def lookupUser(response: play.api.libs.json.JsValue) = {
    val identity_provider = oauth2.auth_identity_provider
    val email = (response \ oauth2.auth_email_field).as[String]
    val sub = (response \ oauth2.auth_sub_field).as[String]
    val name = (response \ oauth2.auth_name_field).as[String]
    val check_user = User.findByIdentity(identity_provider + sub)
    val user = if (check_user == None) {
      // create them
      User.make(name, email, "", identity_provider + sub)
    } else {
      // get the user
      // todo: write tests to show what happens if this user is invalid
      check_user.get
    }
    Redirect(loginDestination(user)).
      withSession("connected" -> user.identity,
                  "subscriber" -> currentSubscriber(user)).flashing(
        "success" -> "You are logged in. Welcome!"
      )
  }

  private def loginDestination(user: User) = {
    if(currentSubscriber(user) != "0") {
      "/dashboard"
    } else if(user.hasRole("analyst")) {
      "/workbench"
    } else if(currentSubscriber(user) == "0") {
      "/subscribers/create"
    } else {
      "/"
    }
  }

  def currentSubscriber(user: User) = {
    val s = Subscriber.findByUserId(user.id)
    if (s.size == 0 ) {
      "0"
    } else {
      s.head.id.toString
    }
  }
}
