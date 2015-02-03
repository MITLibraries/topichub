/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import scala.collection.immutable.HashMap

import java.io.ByteArrayInputStream

import akka.actor.Actor

import play.api.Play.current
import play.api.libs.ws._
//import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.xml.pull._

import models.{Item, PackageMap}

/** Packager is a worker that assembles content packages according
  * to the specifications in a packageMap
  *
  * @author richardrodgers
  */

class PackageWorker extends Actor {
  def receive = {
    case _ => Packager.packageItem
  }
}

class Packager {

  def packageItem(item: Item, pkgMap: PackageMap) = {
    // check the package cache - if already created, skip
    if (! Packager.inCache("foo" /*item, pkgMap.pkgmapId */)) {
      // build a new package & cache it
      Packager.toCache("foo", "bar" /*buildPackage(item, pkgMap) */)
    }
    Packager.fromCache("foo")
  }

  def buildPackage(item: Item, pkgMap: PackageMap) = {

    // add each component in the map and put in a zip file
    def parse(xml: XMLEventReader) = {
      var objId: Option[String] = None
      var readingId = false
      var readingSpec = false
      while (xml.hasNext) {
        xml.next match {
          case EvElemStart(_,"identifier",_,_) => readingId = true
          case EvElemStart(_,"setSpec",_,_) => readingSpec = true
          case EvText(text) if readingId => objId = Some(text); readingId = false
          case EvText(text) if readingSpec => checkItem(objId, Some(text)); readingSpec = false
          case _ =>
        }
      }
    }

    def checkItem(objId: Option[String], collectionKey: Option[String]) = {
      println("Got OID:" + objId.getOrElse("Unknown") + " in coll: " + collectionKey.getOrElse("Unknown"))
    }
  }
}

object Packager {

  // temporary memory cache - will be moved to a persistent cache
  var pkgCache: Map[String, String] = new HashMap

  def inCache(key: String) = pkgCache.contains(key)

  def toCache(key: String, obj: String) = pkgCache += key -> obj

  def fromCache(key: String) = pkgCache.get(key)

  def packageItem = {
    new Packager().packageItem(null, null)
  }
}
