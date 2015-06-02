/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import java.io.ByteArrayInputStream

import akka.actor.Actor

import play.api.Play.current
import play.api.libs.concurrent.Akka
import play.api.libs.ws._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.xml.pull._

import akka.actor.Props

import models.{Collection, Harvest, HubUtils, Item}

/** Harvester is a worker responsible for performing content harvests
  * Only OAI-PMH currently supported
  *
  * @author richardrodgers
  */

class HarvestWorker extends Actor {
  def receive = {
    case h: Harvest => new Harvester().harvest(h)
    case (oid: String, c: Collection, h: Harvest, force: Boolean) => new Harvester().pullItem(oid, c, h, force)
    case _ => println("Unknown")
  }
}

class Harvester {

  def harvest(harvest: Harvest) = {
    // based on protocol, set up harvest
    harvest.protocol.toLowerCase match {
      case "oai-pmh" => oaiHarvest(harvest)
      case _ => println("Unknown protocol")
    }
  }

  def pullItem(oid: String, coll: Collection, harvest: Harvest, force: Boolean) = {
    // TODO - verify OID validity at harvest site
    val resUrl = harvest.resourceUrl.replace("${recordId}", oid.substring(oid.lastIndexOf(":") + 1))
    val curItem = Item.findByKey(oid)
    if (force && curItem.isDefined) Item.delete(curItem.get.id)
    val item = Item.make(coll.id, coll.ctypeId, "remote:" + resUrl, oid)
    if (curItem.isEmpty) coll.recordDeposit
    Harvester.cataloger ! item
  }

  def oaiHarvest(harvest: Harvest) = {

    def parse(xml: XMLEventReader) = {
      var objId: Option[String] = None
      var readingId = false
      var readingSpec = false
      while (xml.hasNext) {
        xml.next match {
          case EvElemStart(_,"identifier",_,_) => readingId = true
          case EvElemStart(_,"setSpec",_,_) => readingSpec = true
          case EvText(text) if readingId => objId = Some(text); readingId = false
          case EvText(text) if readingSpec => processItem(objId, Some(text)); readingSpec = false
          case _ =>
        }
      }
    }

    def processItem(objId: Option[String], collectionKey: Option[String]) = {
      println("Got OID:" + objId.getOrElse("Unknown") + " in coll: " + collectionKey.getOrElse("Unknown"))
      // look up collection, and process if known & item not already created
      val collOpt = Collection.findByTag(collectionKey.get);
      if (collOpt.isDefined && Item.findByKey(objId.get).isEmpty) {
        // create an Item and send to cataloger worker
        val coll = collOpt.get
        val oid = objId.get
        // some fragile assumptions follow
        val resUrl = harvest.resourceUrl.replace("${recordId}", oid.substring(oid.lastIndexOf(":") + 1))
        val item = Item.make(coll.id, coll.ctypeId, "remote:" + resUrl, oid)
        coll.recordDeposit
        Harvester.cataloger ! item
      }
    }

    // debug
    println("About to call: " + harvest.harvestUrl)
    // todo: handle timeouts here by setting the harvest as failed or something to let us know
    // we need to try this date range again.
    WS.url(harvest.harvestUrl).get().map { response =>
      parse(new XMLEventReader(Source.fromInputStream(
            new ByteArrayInputStream(response.body.getBytes))))
    }
  }
}

object Harvester {
  val cataloger = Akka.system.actorOf(Props[CatalogWorker], name="cataloger")
}
