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
    case _ => Logger.error("Unhandled Case in IndexWorker#receive")
  }
}

object Indexer {

  import play.api.libs.json.Json._

  val indexSvc = Play.configuration.getString("hub.index.url").get
  val indexUser = Play.configuration.getString("hub.index.username").getOrElse("")
  val indexPwd = Play.configuration.getString("hub.index.password").getOrElse("")

  def reindex(dtype: String) = {
    // delete current index type
    deindex_all(dtype)

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
    val elastic_url = indexSvc.concat("topic/").concat(topic.id.toString)
    indexDocument(elastic_url, stringify(toJson(data)))
    Logger.debug("Topic index: " + data)
    // sleep required to prevent elasticsearch service from dropping records
    Thread sleep 100
  }

  def deindex(topic: Topic) = {
    deleteDocument(indexSvc.concat("topic/").concat(topic.id.toString))
  }

  def deindex_all(dtype: String) = {
    if (indexUser != "" && indexPwd != "") {
      Logger.debug("Use basic auth for WS elasticsearch call")
      WS.url(indexSvc + dtype).withAuth(indexUser, indexPwd, WSAuthScheme.BASIC).delete()
      WS.url(indexSvc + dtype).withAuth(indexUser, indexPwd, WSAuthScheme.BASIC).withMethod("PUT").stream
    } else {
      Logger.debug("No auth for WS elasticsearch call")
      WS.url(indexSvc + dtype).delete()
      WS.url(indexSvc + dtype).withMethod("PUT").stream
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
    Logger.debug(dataMap.toString)
    indexDocument(elastic_url, stringify(toJson(dataMap)))
    Logger.debug("Item index: " + dataMap)
    // sleep required to prevent elasticsearch service from dropping records
    Thread sleep 100
  }

  def deindex(item: Item) = {
    deleteDocument(indexSvc.concat("item/").concat(item.id.toString))
  }

  private def indexDocument(url: String, jdata: String) = {
    if (indexUser != "" && indexPwd != "") {
      Logger.debug("Use basic auth for WS elasticsearch call")
      WS.url(url).withAuth(indexUser, indexPwd, WSAuthScheme.BASIC).put(jdata)
    } else {
      Logger.debug("No auth for WS elasticsearch call")
      WS.url(url).put(jdata)
    }
  }

  private def deleteDocument(url: String) = {
    if (indexUser != "" && indexPwd != "") {
      Logger.debug("Use basic auth for WS elasticsearch call")
      WS.url(url).withAuth(indexUser, indexPwd, WSAuthScheme.BASIC).delete
    } else {
      Logger.debug("No auth for WS elasticsearch call")
      WS.url(url).delete
    }
  }

  private def addMetadata(scheme: Scheme, item: Item) = {
    // NB: need logic here to test whether scheme is metadata or not
    scheme.tag -> toJson(item.metadataValue(scheme.tag))
  }
}
