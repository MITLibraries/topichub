/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package controllers

import org.joda.time.DateTimeZone
import org.joda.time.format.ISODateTimeFormat

import play.api.mvc._
import play.api._
import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits._
import play.utils.UriEncoding

import models.HubUtils._
import models.{HubUtils, Item, Subscriber, Topic}

/**
 * Search controller manages search pages in the UI and protocol
 * requests from OpenSearch operations
 * TODO - add accessUrls?
 */

object Search extends Controller {

  val indexSvc = Play.configuration.getString("hub.index.url").get
  val indexUser = Play.configuration.getString("hub.index.username").getOrElse("")
  val indexPwd = Play.configuration.getString("hub.index.password").getOrElse("")
  val adminEmail = Play.configuration.getString("hub.admin.email").get
  val iso8601 = ISODateTimeFormat.dateTimeNoMillis.withZone(DateTimeZone.UTC)

  def index = Action { implicit request =>
    Ok(views.html.search.index())
  }

  def results(q: String, target: String, page: Int, perpage: Int, format: String) = Action.async {
    implicit request =>
    val req = makeRequest(q, target, page, perpage)
    req.get().map { response =>
      val json = response.json
      val total_results = (json \ "hits" \\ "total")(0).as[Long]
      val hits = (json \ "hits" \\ "hits").head \\ "dbId" map(_.as[Int])
      if (target == "item") {
        val items = hits flatMap ( id => Item.findById(id) )
        if ("html" == format) {
          Ok(views.html.search.item_results(q, target, page, perpage, items, total_results))
        } else { // at this point, whatever else they want, they're gonna get Atom regardless
          itemResultsAtom(q, page, perpage, items, total_results)
        }
      } else if (target == "topic") {
        val topics = hits flatMap ( id => Topic.findById(id) )
        val sub = Subscriber.findById(Application.currentSubscriberId)
        Ok(views.html.search.topic_results(q, target, page, perpage, topics, total_results, sub))
      } else {
        NotFound(views.html.static.trouble("Only Topic or Item searching are supported."))
      }
    }
  }

  def openSearchDescription(dtype: String) = Action { implicit request =>
    def templateUrl(ftype: String) = {
      routes.Search.index.absoluteURL + "/results?q={searchTerms}&page={startPage?}&perpage={count?}" +
      "&target=" + dtype + "&format=" + ftype
    }
    Ok(
      <OpenSearchDescription xmlns="http://a9.com/-/spec/opensearch/1.1/">
        <ShortName>TopicHub Search</ShortName>
        <LongName>{HubUtils.siteName.take(48)}</LongName>
        <Description>Search for {dtype}s currently on the hub</Description>
        <Contact>{adminEmail}</Contact>
        <Image height="16" width="16" type="image/vnd.microsoft.icon">{routes.Assets.at("images/favicon.png").absoluteURL}</Image>
        <Url type="text/html" template={templateUrl("html")}/>
        <Url type="application/atom+xml" template={templateUrl("atom")}/>
      </OpenSearchDescription>
    ).as("application/opensearchdescription+xml")
  }

  def itemResultsAtom(q: String, page: Int, perpage: Int, items: Seq[Item], total: Long)(implicit request: RequestHeader) = {
    val lastPage = if (total.toInt % perpage > 0) (total.toInt / perpage) else (total.toInt / perpage) - 1
    def schemeUrl(id: Int) = routes.Application.scheme(id).absoluteURL
    def doiUrl(item: Item) = "http://doi.org/" + item.metadataValue("doi")
    Ok(
      <feed xmlns="http://www.w3.org/2005/Atom"
            xmlns:opensearch="http://a9.com/-/spec/opensearch/1.1/">
        <title>{HubUtils.siteName}</title>
        <id>{routes.Application.index.absoluteURL}</id>
        <updated>{iso8601.print(System.currentTimeMillis)}</updated>
        <opensearch:itemsPerPage>{perpage}</opensearch:itemsPerPage>
        <opensearch:totalResults>{total}</opensearch:totalResults>
        <opensearch:Query role="request" searchTerms={q} startPage={page.toString}/>
        <link rel="self" type="application/atom+xml"
              href={routes.Search.results(q, "item", page, perpage, "atom").absoluteURL}/>
        <link rel="first" type="application/atom+xml"
              href={routes.Search.results(q, "item", 0, perpage, "atom").absoluteURL}/>
        { if (page > 0)
          <link rel="previous" type="application/atom+xml"
                href={routes.Search.results(q, "item", page - 1, perpage, "atom").absoluteURL}/>
        }
        { if (page < lastPage)
          <link rel="next" type="application/atom+xml"
                href={routes.Search.results(q, "item", page + 1, perpage, "atom").absoluteURL}/>
        }
        <link rel="last" type="application/atom+xml"
              href={routes.Search.results(q, "item", lastPage, perpage, "atom").absoluteURL}/>
        <link rel="search" type="application/opensearchdescription+xml"
              href={routes.Search.openSearchDescription("item").absoluteURL}/>
        { for (item <- items) yield
          <entry>
            <title>{item.metadataValue("title")}</title>
            <id>{routes.ItemController.item(item.id).absoluteURL}</id>
            { if (item.hasMetadata("doi"))
              <link rel="alternate" href={doiUrl(item)}/>
            }
            <updated>{iso8601.print(item.created.getTime())}</updated>
            { for (auth <- item.metadataValues("authors")) yield
              <author>
                <name>{auth}</name>
              </author>
            }
            { for (topic <- item.topics if topic.scheme.tag != "meta") yield
              <category term={topic.tag} scheme={schemeUrl(topic.scheme_id)} label={topic.name}/>
            }
            <summary type="text">
              {item.metadataValue("abstract")}
            </summary>
          </entry>
        }
      </feed>
    ).as("application/atom+xml")
  }

  private def makeRequest(query: String, target: String, page: Int, perpage: Int): WSRequest = {
    val encQuery = UriEncoding.encodePathSegment(query, "UTF-8")
    val offset = (page) * perpage
    val elastic_url = indexSvc +  target + "/_search?q=" + encQuery + "&from=" + offset + "&size=" + perpage
    if (indexUser != "" && indexPwd != "") {
      Logger.debug("use basic auth for WS elasticsearch call")
      WS.url(elastic_url).withAuth(indexUser, indexPwd, WSAuthScheme.BASIC)
    } else {
      Logger.debug("no auth for WS elasticsearch call")
      WS.url(elastic_url)
    }
  }
}
