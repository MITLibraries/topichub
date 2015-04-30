package controllers

import play.api.mvc._
import play.api._
import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits._
import play.utils.UriEncoding
import models.HubUtils._
import models._

object Search extends Controller {

  def index = Action { implicit request =>
    Ok(views.html.search.index())
  }

  def results(q: String, target: String, page: Int, perpage: Int) = Action.async {
    implicit request =>
    val indexSvc = Play.configuration.getString("hub.index.url").get
    val encQuery = UriEncoding.encodePathSegment(q, "UTF-8")
    val offset = (page) * perpage
    val elastic_url = indexSvc +  target + "/_search?q=" + encQuery + "&from=" + offset + "&size=" + perpage
    val req = if (indexSvc.contains("bonsai.io")) {
      println("DEBUG: use basic auth for WS elasticsearch call")
      WS.url(elastic_url)
        .withAuth(extractCredentials("username", indexSvc),
                  extractCredentials("password", indexSvc),
                  WSAuthScheme.BASIC)
    } else {
      println("DEBUG: no auth for WS elasticsearch call")
      WS.url(elastic_url)
    }

    req.get().map { response =>
      val json = response.json
      val total_results = (json \ "hits" \\ "total")(0).as[Long]
      val hits = (json \ "hits" \\ "hits").head \\ "dbId" map(_.as[Int])
      if (target == "item") {
        val items = hits flatMap ( id => Item.findById(id) )
        Ok(views.html.search.item_results(q, target, page, perpage, items, total_results))
      } else if (target == "topic") {
        val topics = hits flatMap ( id => Topic.findById(id) )
        val sub = Subscriber.findById(Application.currentSubscriberId)
        Ok(views.html.search.topic_results(q, target, page, perpage, topics, total_results, sub))
      } else {
        NotFound(views.html.static.trouble("Only Topic or Item searching are supported."))
      }
    }
  }
}
