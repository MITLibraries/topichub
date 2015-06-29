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
import scala.util.control.Breaks._

import akka.actor.Props

import models.{Collection, Harvest, HubUtils, Item, User}
import services.Emailer

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
    var unhandledCollections = scala.collection.mutable.ListBuffer.empty[String]

    def parse(xml: XMLEventReader) = {
      var objId: Option[String] = None
      var readingId = false
      var readingSpec = false
      var onError = false
      var attributes = ""
      var oaiDetected = false
      var counter = 0
      while (xml.hasNext) {
        xml.next match {
          case EvElemStart(_,"OAI-PMH",_,_) => oaiDetected = true
          case EvElemStart(_,"error",attrs,_) => onError = true; attributes = attrs.toString
          case EvElemStart(_,"identifier",_,_) => readingId = true
          case EvElemStart(_,"setSpec",_,_) => readingSpec = true
          case EvText(text) if onError => onError = false;
                                          handleOaiError(text, attributes); attributes = ""
          case EvText(text) if readingId => objId = Some(text); readingId = false
          case EvText(text) if readingSpec => processItem(objId, Some(text)); readingSpec = false
          case _ => if (oaiDetected == false && counter > 2) { abortHarvest("OAI xml not detected."); break } else { counter = counter + 1 }
        }
      }
      if (unhandledCollections.toList.distinct.size > 0) {
        notifyUnhandledCollections(unhandledCollections.toList.distinct)
      }
    }

    def notifyUnhandledCollections(collections: List[String]) = {
      val sysadminEmails = User.allByRole("sysadmin").map(x => x.email).mkString(",")
      val msg = views.txt.email.unhandled_collections(harvest, collections).body
      println(msg)
      Emailer.notify(sysadminEmails, "SCOAP3Hub: An unhandled collection was detected", msg)
    }

    def handleOaiError(errorText: String, errorCode: String) = {
      if(errorCode.contains("noRecordsMatch")) {
        println("DEBUG: No records matched, but don't abort the Harvest because that's just fine.")
      } else {
        // Any other errorCode aborts which will generate an email to sysadmins with error details
        abortHarvest(errorCode + " " + errorText)
      }
    }

    def processItem(objId: Option[String], collectionKey: Option[String]) = {
      println("Got OID:" + objId.getOrElse("Unknown") + " in coll: " + collectionKey.getOrElse("Unknown"))
      // look up collection, and process if known & item not already created
      val collOpt = Collection.findByTag(collectionKey.get)
      if (collOpt.isDefined && Item.findByKey(objId.get).isEmpty) {
        // create an Item and send to cataloger worker
        val coll = collOpt.get
        val oid = objId.get
        // some fragile assumptions follow
        val resUrl = harvest.resourceUrl.replace("${recordId}", oid.substring(oid.lastIndexOf(":") + 1))
        val item = Item.make(coll.id, coll.ctypeId, "remote:" + resUrl, oid)
        coll.recordDeposit
        Harvester.cataloger ! item
      } else if (collOpt.isDefined) {
        println("DEBUG: collection is defined but Item is already cataloged")
      } else if (Collection.findByTag(collectionKey.get, false).isDefined) {
        println(s"DEBUG: Collection is ignored: ${collectionKey.get}")
      } else {
        // keep track so we can send an email so someone knows a new collection was found
        println(s"DEBUG: Collection is not handled: ${collectionKey.get}")
        unhandledCollections += collectionKey.get
      }
    }

    def abortHarvest(exception: String) = {
      Harvest.findById(harvest.id).get.rollback
      val sysadminEmails = User.allByRole("sysadmin").map(x => x.email).mkString(",")
      val msg = views.txt.email.abort_harvest(harvest, exception).body
      println(msg)
      Emailer.notify(sysadminEmails, "SCOAP3Hub: An error occurred starting a Harvest", msg)
    }

    // OAI-PMH date filters are inclusive on both ends (from and until),
    // so same from and until = 1 day. Thus a harvest starts from 1 day
    // past the last updated date thru freqency - 1 additional days (clear as mud?)
    val from = HubUtils.advanceDate(harvest.updated, 1)
    val until = HubUtils.advanceDate(from, harvest.freq - 1)
    val url = harvest.serviceUrl + "?verb=ListIdentifiers&metadataPrefix=oai_dc" +
                                   "&from=" + HubUtils.fmtDate(from) +
                                   "&until=" + HubUtils.fmtDate(until)
    // To test handling of no records found, you can set url as below.
    // val url = "http://repo.scoap3.org/oai2d?verb=ListIdentifiers&metadataPrefix=oai_dc&from=2012-05-04&until=2012-05-04"
    // To test non-xml
    // val url = "http://www.google.com"
    // To test xml with no OAI
    // val url = "http://api.npr.org/query?id=1029&apiKey=MDE5MDcxMDkzMDE0MzA5MjI3NDgxNDRkMA001"
    // debug
    println("About to call: " + url)

    val request = WS.url(url).get().map { response =>
      response.status match {
        case 200 => {
          if (response.allHeaders("Content-Type").mkString.contains("text/xml")) {
            parse(new XMLEventReader(Source.fromInputStream(
              new ByteArrayInputStream(response.body.getBytes))))
          } else {
            abortHarvest(s"""Got Content-Type ${response.allHeaders("Content-Type").mkString}.
                             |Expected that should contain 'text/xml' when requesting ${url}"""
                             .stripMargin)
          }
        }
        case _ => {
          abortHarvest(s"${response.status} response code received when requesting ${url}")
        }
      }
    }

    request.recover {
      case e: Exception => abortHarvest(e.toString)
   }
  }
}

object Harvester {
  val cataloger = Akka.system.actorOf(Props[CatalogWorker], name="cataloger")
}
