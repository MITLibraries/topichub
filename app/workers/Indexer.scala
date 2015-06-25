/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import akka.actor.Actor

import play.api._
import play.api.Play.current
//import play.api.libs.concurrent.Akka
import play.api.libs.json._
import play.api.libs.json.Json
import play.api.libs.ws._
import models.HubUtils._
import models.{ContentType, Item, Scheme, Topic}

/** Indexer is a worker responsible for updating the index
  * of hub content
  *
  * @author richardrodgers
  */

class IndexWorker extends Actor {
  def receive = {
    case item: Item => Indexer.index(item)
    case topic: Topic => Indexer.index(topic)
    //case subscriber: Subscriber => Indexer.index(subscriber)
    case dtype: String => Indexer.reindex(dtype)
    case _ => println("I'm lost")
  }
}

object Indexer {

  import play.api.libs.json.Json._

  val indexSvc = Play.configuration.getString("hub.index.url").get

  def reindex(dtype: String) = {
    // delete current index type
    if (indexSvc.contains("bonsai.io")) {
      // println("DEBUG: use basic auth for WS elasticsearch call")
      WS.url(indexSvc + dtype)
        .withAuth(extractCredentials("username", indexSvc),
                  extractCredentials("password", indexSvc),
                  WSAuthScheme.BASIC).delete()
    } else {
      // println("DEBUG: no auth for WS elasticsearch call")
      WS.url(indexSvc + dtype).delete()
    }

    if ("topic".equals(dtype)) {
      Topic.all.foreach(index(_))
    } else if ("item".equals(dtype)) {
      Item.all.foreach(index(_))
    }
  }

  def index(topic: Topic) = {
    // minimal indexing: dbId, schemeId, tag, and name
    val data = Map("dbId" -> toJson(topic.id),
                   "schemeTag" -> toJson(topic.scheme.tag),
                   "tag" -> toJson(topic.tag),
                   "name" -> toJson(topic.name))
    val jdata = stringify(toJson(data))
    val elastic_url = indexSvc.concat("topic/").concat(topic.id.toString)
    // debug
    // println("Topic index: " + jdata)

    if (indexSvc.contains("bonsai.io")) {
      // println("DEBUG: use basic auth for WS elasticsearch call")
      WS.url(elastic_url)
        .withAuth(extractCredentials("username", indexSvc),
                  extractCredentials("password", indexSvc),
                  WSAuthScheme.BASIC).put(jdata)
    } else {
      // println("DEBUG: no auth for WS elasticsearch call")
      WS.url(elastic_url).put(jdata)
    }
  }

  def index(item: Item) = {
    var dataMap = Map[String, JsValue]()
    val ctype = ContentType.findById(item.ctypeId).get
    val elastic_url = indexSvc.concat("item/").concat(item.id.toString)
    dataMap += "dbId" -> toJson(item.id)
    // add all defined index fields
    ctype.schemes("index").foreach(dataMap += addMetadata(_, item))
    // also add all topics
    dataMap += "topicSchemeTag" -> toJson(item.topics.map(_.scheme.tag))
    dataMap += "topicTag" -> toJson(item.topics.map(_.tag))
    val jdata = stringify(toJson(dataMap))
    // println("Item index: " + dataMap)
    // println(indexSvc + "item/" + item.id)

    if (indexSvc.contains("bonsai.io")) {
      // println("DEBUG: use basic auth for WS elasticsearch call")
      WS.url(elastic_url)
        .withAuth(extractCredentials("username", indexSvc),
                  extractCredentials("password", indexSvc),
                  WSAuthScheme.BASIC).put(jdata)
    } else {
      // println("DEBUG: no auth for WS elasticsearch call")
      WS.url(elastic_url).put(jdata)
    }
  }

  private def addMetadata(scheme: Scheme, item: Item) = {
    // NB: need logic here to test whether scheme is metadata or not
    scheme.tag -> toJson(item.metadataValue(scheme.tag))
  }

  /*
  def index(subscriber: Subscriber) = {
    // minimal indexing: dbId, schemeId, topicId, and title
    val data = Map("dbId" -> toJson(subscriber.id),
                   "category" -> toJson(subscriber.category),
                   "keywords" -> toJson(subscriber.keywords),
                   "name" -> toJson(subscriber.name))
    val jdata = stringify(toJson(data))
    // debug
    println("Subscriber index: " + jdata)
    val req = WS.url(indexSvc + "subscriber/" + subscriber.id)
    req.put(jdata)
  }
  */
}
