package controllers

import play.api.mvc._
import play.api._
import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits._
import play.utils.UriEncoding
import models.HubUtils._
import models._
import java.util.UUID
import services.OAuth2
import play.api.mvc.Security._

object AuthenticationController extends Controller {

  def login = Action { implicit request =>
    val oauth2 = new OAuth2(Play.current)
    val callbackUrl = services.routes.OAuth2.callback(None, None).absoluteURL()
    val scope = "openid email profile"   // this is used auth the ticket to provide info we need later
    val state = UUID.randomUUID().toString  // random confirmation string
    val redirectUrl = oauth2.getAuthorizationUrl(callbackUrl, scope, state)
    // todo: withSession used like this actually logs the user out
    // which sort of makes sense actually. However, what may make
    // sense would be to redirect the user if they are logged in
    // so we never hit this line that logs them out again.
    Ok(views.html.login.index("", redirectUrl)).
      withSession("oauth-state" -> state)
  }
}

trait Security {
  def identity(request: RequestHeader) = {
    if (request.session.get("connected") != None) {
      if (User.isValidIdentity(request.session.get("connected").get)) {
        User.findByIdentity(request.session.get("connected").get)
      } else {
        None
      }
    } else {
      None
    }
  }

  def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.AuthenticationController.login)

  def isAuthenticated(f: => User => Request[AnyContent] => Result) = {
    Authenticated(identity, onUnauthorized) { user =>
      Action(request => f(user)(request))
    }
  }

  def isAnalyst(f: => User => Request[AnyContent] => Result) = {
    Authenticated(identity, onUnauthorized) { user =>
      if (hasRole(user, "analyst")) {
        Action(request => f(user)(request))
      } else {
        Action(request =>
          Results.Unauthorized(views.html.static.trouble("You are not authorized")))
      }
    }
  }

  def isAdmin(f: => User => Request[AnyContent] => Result) = {
    Authenticated(identity, onUnauthorized) { user =>
      if (hasRole(user, "sysadmin")) {
        Action(request => f(user)(request))
      } else {
        Action(request =>
          Results.Unauthorized(views.html.static.trouble("You are not authorized")))
      }
    }
  }

  def hasRole(user: User, role: String) = {
    if (user.hasRole(role)) {
      true
    } else {
      false
    }
  }
}
