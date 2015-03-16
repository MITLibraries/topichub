package controllers

import play.api.mvc._
import play.api._
import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits._
import play.utils.UriEncoding
import models.HubUtils._
import models._
import play.api.libs.json._

object Search extends Controller {

  def index = Action {
    Ok(views.html.search.index())
  }

  private def elastic_xml(q: String, offset: Int, perpage: Int) = {
      s"""
      {
        "from" : ${offset}, "size" : ${perpage},
        "query" : {
          "match" : {
            "_all" : {
                "query" : "${q}",
                "type" : "phrase"
            }
          }
        }
      }
      """
  }

  def results(q: String, target: String, substatus: String, page: Int, perpage: Int) = Action.async {
    val indexSvc = Play.configuration.getString("hub.index.url").get
    val encQuery = UriEncoding.encodePathSegment(q, "UTF-8")
    val offset = (page) * perpage
    val elastic_url = indexSvc +  target + "/_search"
    //val search_json = Json.toJson(elastic_xml(encQuery))
    val search_json = elastic_xml(q, offset, perpage)
    println(search_json)

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

    println(req)

    req.post(search_json).map { response =>
      val json = response.json
      println(json)
      val total_results = (json \ "hits" \\ "total")(0).as[Long]

      println(total_results)
      val hits = (json \ "hits" \\ "hits").head \\ "dbId" map(_.as[Int])
      println("Size:" + hits.size)
      hits foreach(x => println(x))

      if (target == "item") {
        val items = hits flatMap ( id => Item.findById(id) )
        Ok(views.html.search.item_results(q, target, page, perpage, items, total_results, substatus))
      } else if (target == "topic") {
        // this seems to be doing n queries whereas a it could be done with a single query
        // if we created a method for Topic.findByIds that accepted a List of IDs
        println(substatus)
        val topics = hits flatMap ( id => Topic.findById(id, substatus, Subscriber.findByUserId(1).get.id) )
        Ok(views.html.search.topic_results(q, target, page, perpage, topics, total_results, substatus, Subscriber.findByUserId(1)))
      } else {
        NotFound(views.html.static.trouble("Only Topic or Item searching are supported."))
      }
    }
  }
}
