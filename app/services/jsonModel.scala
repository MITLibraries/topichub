/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package services

import java.util.Date

import play.api._
import play.api.libs.json._
import play.api.libs.json.Json._
import play.api.Play.current

import models._

import jsonHelpers._

/** Utility methods for serializing and deserializing various data models as
  * JSON objects. Currently supported: content, publisher and subscriber models
  *
  * @author richardrodgers
  */

object contentModelJson {

  // serialization methods
  def jsonContentModel = {
    val model = Map(
      "cformats" -> jsonContentFormats,
      "schemes" -> jsonSchemes,
      "ctypes" -> jsonContentTypes,
      "resmaps" -> jsonResourceMaps
    )
    Json.stringify(toJson(model))
  }

  def prettyContentModel = {
    val model = Map(
      "cformats" -> jsonContentFormats,
      "schemes" -> jsonSchemes,
      "ctypes" -> jsonContentTypes,
      "resmaps" -> jsonResourceMaps
    )
    Json.prettyPrint(toJson(model))
  }

  def jsonSchemes = {
    val msg = Scheme.all.map( s =>
      Map("tag" -> toJson(s.tag),
          "gentype"  -> toJson(s.gentype),
          "category" -> toJson(s.category),
          "description" -> toJson(s.description),
          "link" -> toJson(s.link),
          "logo" -> toJson(s.logo),
          "finders" -> jsonFinders(s.id),
          "validators" -> jsonValidators(s.id)
      )
    )
    toJson(msg)
  }

  def jsonFinders(sid: Int) = {
    val msg = Finder.findByScheme(sid).map ( f =>
      Map("description" -> toJson(f.description),
          "cardinality" -> toJson(f.cardinality),
          "format" -> toJson(f.format.get.tag),
          "idKey" -> toJson(f.idKey),
          "idLabel" -> toJson(f.idLabel),
          "author" -> toJson(f.author)
      )
    )
    toJson(msg)
  }

  def jsonValidators(sid: Int) = {
    val msg = Validator.findByScheme(sid).map ( v =>
      Map("description" -> toJson(v.description),
          "userId" -> toJson(v.userId),
          "password" -> toJson(v.password),
          "serviceCode" -> toJson(v.serviceCode),
          "serviceUrl" -> toJson(v.serviceUrl),
          "author" -> toJson(v.author)
      )
    )
    toJson(msg)
  }

  def jsonContentTypes = {
    val msg = ContentType.all.map( t =>
      Map("tag" -> toJson(t.tag),
          "label" -> toJson(t.label),
          "description" -> toJson(t.description),
          "logo" -> toJson(t.logo),
          "meta" -> toJson(t.schemes("meta").map(s => s.tag)),
          "index" -> toJson(t.schemes("index").map(s => s.tag)),
          "topic" -> toJson(t.schemes("topic").map(s => s.tag))
      )
    )
    toJson(msg)
  }

  def jsonContentFormats = {
    val msg = ContentFormat.all.map( f =>
      Map("tag" -> toJson(f.tag),
          "label" -> toJson(f.label),
          "description" -> toJson(f.description),
          "url" -> toJson(f.url),
          "mimetype" -> toJson(f.mimetype),
          "logo" -> toJson(f.logo)
      )
    )
    toJson(msg)
  }

  def jsonResourceMaps = {
    val msg = ResourceMap.all.map( m =>
      Map("tag" -> toJson(m.tag),
          "description" -> toJson(m.description),
          "swordUrl" -> toJson(m.swordUrl),
          "mappings" -> jsonResourceMappings(m)
      )
    )
    toJson(msg)
  }

  def jsonResourceMappings(resMap: ResourceMap) = {
    val msg = resMap.schemes.map(s =>
      Map ("scheme" -> toJson(s.tag),
           "maps" -> jsonResourceSchemeMappings(resMap, s))
      )
    toJson(msg)
  }

  def jsonResourceSchemeMappings(resMap: ResourceMap, scheme: Scheme) = {
    val msg = resMap.mappingsForScheme(scheme).map( m =>
      Map("source" -> toJson(m._1),
          "format" -> toJson(ContentFormat.findById(m._2).get.tag),
          "rank" -> toJson(m._3)
      )
    )
    toJson(msg)
  }

  // deserialization methods
  def buildContentModel(model: JsValue) = {
    val formats = (model \ "cformats")
    procJsArray(formats, 0, cformatFromContentModel)
    println("finished formats")
    val schemes = (model \ "schemes")
    procJsArray(schemes, 0, schemeFromContentModel)
    println("finished schemes")
    val ctypes = (model \ "ctypes")
    procJsArray(ctypes, 0, ctypeFromContentModel)
    println("finished ctypes")
    val resmaps = (model \ "resmaps")
    procJsArray(resmaps, 0, resmapFromContentModel)
    println("finished resmaps")
  }

  def cformatFromContentModel(jss: JsValue) {
    val tag = forName(jss, "tag")
    // only create if not already defined
    if (ContentFormat.findByTag(tag).isEmpty) {
      ContentFormat.create(tag, forName(jss, "label"), forName(jss, "description"),
                           forName(jss, "url"), forName(jss, "mimetype"),
                           forNameOption(jss, "logo"))
    }
  }

  def schemeFromContentModel(jss: JsValue) {
    //println(Json.stringify(jss))
    val tag = forName(jss, "tag")
    // only create if not already defined
    if (Scheme.findByTag(tag).isEmpty) {
      Scheme.create(tag, forName(jss, "gentype"), forName(jss, "category"),
                    forName(jss, "description"), forNameOption(jss, "link"),
                    forNameOption(jss, "logo"))
      val scheme = Scheme.findByTag(tag).get
      val finders = (jss \ "finders")
      procJsArray(finders, 0, finderFromContentModel(scheme.id))
      val validators = (jss \ "validators")
      procJsArray(validators, 0, validatorFromContentModel(scheme.id))
    }
  }

  def finderFromContentModel(scheme_id: Int)(jsf: JsValue) {
    val format = ContentFormat.findByTag(forName(jsf, "format")).get
    // NB: does not check for duplicates properly!
    //if (Finder.forSchemeAndFormat(scheme_id, format.id).isEmpty) {
      Finder.create(scheme_id, format.id, forName(jsf, "description"),
                    forName(jsf, "cardinality"), forName(jsf, "idKey"),
                    forName(jsf, "idLabel"), forName(jsf, "author"))
    //}
  }

  def validatorFromContentModel(scheme_id: Int)(jsf: JsValue) {
    Validator.create(scheme_id, forName(jsf, "description"), forName(jsf, "userId"),
                    forName(jsf, "password"), forName(jsf, "serviceCode"),
                    forName(jsf, "serviceUrl"), forName(jsf, "author"))
  }

  def ctypeFromContentModel(jsc: JsValue) {
    val tag = forName(jsc, "tag")
    // only create if not already defined
    if (ContentType.findByTag(tag).isEmpty) {
      ContentType.create(tag, forName(jsc, "label"), forName(jsc, "description"),
                         forNameOption(jsc, "logo"))
      val ctype = ContentType.findByTag(tag).get
      procJsArray((jsc \ "meta"), 0, ctMapFromCmodel(ctype, "meta"))
      procJsArray((jsc \ "index"), 0, ctMapFromCmodel(ctype, "index"))
      procJsArray((jsc \ "topic"), 0, ctMapFromCmodel(ctype, "topic"))
    }
  }

  def ctMapFromCmodel(ctype: ContentType, reln: String)(jsc: JsValue) {
    val scheme = Scheme.findByTag(jsc.as[String]).get
    ctype.addScheme(scheme, reln)
  }

  def resmapFromContentModel(jsp: JsValue) {
    val tag = forName(jsp, "tag")
    // only create if not already defined
    if (ResourceMap.findByTag(tag).isEmpty) {
      ResourceMap.create(tag, forName(jsp, "description"), forNameOption(jsp, "swordUrl"))
      val resmap = ResourceMap.findByTag(tag).get
      val mappings = (jsp \ "mappings")
      procJsArray(mappings, 0, rmFromCmodel(resmap))
    }
  }

  def rmFromCmodel(resmap: ResourceMap)(jsc: JsValue) {
    val tag = forName(jsc, "scheme")
    val scheme = Scheme.findByTag(tag).get
    val maps = (jsc \ "maps")
    procJsArray(maps, 0, rmSubMapFromCmodel(resmap, scheme.id))
  }

  def rmSubMapFromCmodel(resmap: ResourceMap, sid: Int)(jsm: JsValue) {
    val fmt = ContentFormat.findByTag(forName(jsm, "format")).get
    resmap.addMapping(sid, fmt.id, forName(jsm, "source"), forNum(jsm, "rank"))
  }
}

object publisherModelJson {

  //serialization methods
  def jsonPublisherModel = {
    val model = Map(
      "publishers" -> jsonPublishers
    )
    Json.stringify(toJson(model))
  }

  def prettyPublisherModel = {
    val model = Map(
      "publishers" -> jsonPublishers
    )
    Json.prettyPrint(toJson(model))
  }

  def jsonPublishers = {
    val msg = Publisher.all.map( p =>
      Map("tag" -> toJson(p.tag),
          "name"  -> toJson(p.name),
          "description" -> toJson(p.description),
          "category" -> toJson(p.category),
          "status" -> toJson(p.status),
          "link" -> toJson(p.link),
          "logo" -> toJson(p.logo),
          "collections" -> jsonCollections(p.id),
          "harvests" -> jsonHarvests(p.id)
      )
    )
    toJson(msg)
  }

  def jsonCollections(pid: Int) = {
    val msg = Collection.findByPublisher(pid).map ( c =>
      Map("ctype" -> toJson(ContentType.findById(c.ctypeId).get.tag),
          "resmap" -> toJson(ResourceMap.findById(c.resmapId).get.tag),
          "tag" -> toJson(c.tag),
          "description" -> toJson(c.description),
          "policy" -> toJson(c.policy)
      )
    )
    toJson(msg)
  }

  def jsonHarvests(pid: Int) = {
    val msg = Harvest.findByPublisher(pid).map ( h =>
      Map("name" -> toJson(h.name),
          "protocol" -> toJson(h.protocol),
          "serviceUrl" -> toJson(h.serviceUrl),
          "resourceUrl" -> toJson(h.resourceUrl),
          "freq" -> toJson(h.freq)
      )
    )
    toJson(msg)
  }

  // deserialization methods
  def buildPublisherModel(model: JsValue) = {
    val pubs = (model \ "publishers")
    procJsArray(pubs, 0, pubFromPublisherModel)
    println("finished publishers")
  }

  def pubFromPublisherModel(jss: JsValue) {
    val tag = forName(jss, "tag")
    // only create if not already defined
    if (Publisher.findByTag(tag).isEmpty) {
      // need to inject an owning user here -dummy value of 1
      Publisher.create(1, tag, forName(jss, "name"), forName(jss, "description"),
                       forName(jss, "category"), forName(jss, "status"),
                       forNameOption(jss, "link"), forNameOption(jss, "logo"))
      val pub = Publisher.findByTag(tag).get
      val colls = (jss \ "collections")
      procJsArray(colls, 0, collFromPublisherModel(pub.id))
      val harvests = (jss \ "harvests")
      procJsArray(harvests, 0, harvestFromPublisherModel(pub.id))
    }
  }

  def collFromPublisherModel(pid: Int)(jss: JsValue) {
    //println(Json.stringify(jss))
    val tag = forName(jss, "tag")
    // only create if not already defined && dependencies found
    if (Collection.findByTag(tag).isEmpty) {
      ContentType.findByTag(forName(jss, "ctype")).map { ctype =>
        ResourceMap.findByTag(forName(jss, "resmap")).map { resmap =>
          Collection.create(pid, ctype.id, resmap.id, tag,
                            forName(jss, "description"), forName(jss, "policy"))
        }
      }
    }
  }

  def harvestFromPublisherModel(pid: Int)(jss: JsValue) {
    // harvests are lightweight and not necessarily unique
    // just create and if they are duplicates, user can delete/ignore
    Harvest.create(pid, forName(jss, "name"), forName(jss, "protocol"),
                   forName(jss, "serviceUrl"), forName(jss, "resourceUrl"),
                   forNum(jss, "freq"), new Date)
  }
}

object subscriberModelJson {

  //serialization methods
  def jsonSubscriberModel = {
    val model = Map(
      "subscribers" -> jsonSubscribers
    )
    Json.stringify(toJson(model))
  }

  def prettySubscriberModel = {
    val model = Map(
      "subscribers" -> jsonSubscribers
    )
    Json.prettyPrint(toJson(model))
  }

  def jsonSubscribers = {
    val msg = Subscriber.all.map( s =>
      Map("name" -> toJson(s.name),
          "category" -> toJson(s.category),
          "contact" -> toJson(s.contact),
          "link" -> toJson(s.link),
          "logo" -> toJson(s.logo),
          "channels" -> jsonChannels(s),
          "plans" -> jsonPlans(s.id),
          "interests" -> jsonInterests(s.id)
      )
    )
    toJson(msg)
  }

  def jsonPlans(sid: Int) = {
    val msg = Plan.findBySubscriber(sid).map ( p =>
      Map("channelUrl" -> toJson(Channel.findById(p.channelId).get.channelUrl),
          "name" -> toJson(p.name),
          "description" -> toJson(p.description),
          "icon" -> toJson(p.icon),
          "fulfill" -> toJson(p.fulfill),
          "pick" -> toJson(p.pick),
          "interest" -> toJson(p.interest),
          "template" -> toJson(p.template),
          "schemes" -> jsonPlanSchemes(p)
      )
    )
    toJson(msg)
  }

  def jsonInterests(sid: Int) = {
    val msg = Interest.findBySubscriber(sid).map ( i =>
      Map("scheme" -> toJson(i.schemeTag),
          "value" -> toJson(i.intValue),
          "template" -> toJson(i.template)
      )
    )
    toJson(msg)
  }

  def jsonPlanSchemes(plan: Plan) = {
    val msg = plan.schemes.map( s =>
      Map("tag" -> toJson(s.tag))
    )
    toJson(msg)
  }

  def jsonChannels(sub: Subscriber) = {
    val msg = sub.channels.map ( c =>
      Map("protocol" -> toJson(c.protocol),
          "mode" -> toJson(c.mode),
          "description" -> toJson(c.description),
          "userId" -> toJson(c.userId),
          "password" -> toJson(c.password),
          "channelUrl" -> toJson(c.channelUrl)
      )
    )
    toJson(msg)
  }

  // deserialization methods
  def buildSubscriberModel(model: JsValue) = {
    val subs = (model \ "subscribers")
    procJsArray(subs, 0, subFromSubscriberModel)
    println("finished subscribers")
  }

  def subFromSubscriberModel(jss: JsValue) {
    // need to inject an owning user here -dummy value of 1
    val sub = Subscriber.make(1, forName(jss, "name"), forName(jss, "category"),
                              forName(jss, "contact"), forNameOption(jss, "link"),
                              forNameOption(jss, "logo"))
    val channels = (jss \ "channels")
    procJsArray(channels, 0, chanFromSubscriberModel(sub.id))
    val plans = (jss \ "plans")
    procJsArray(channels, 0, planFromSubscriberModel(sub.id))
    val interests = (jss \ "interests")
    procJsArray(interests, 0, intFromSubscriberModel(sub.id))
  }

  def intFromSubscriberModel(sid: Int)(jss: JsValue) = {
    Interest.create(sid, forName(jss, "scheme"), forName(jss, "value"), forName(jss, "template").equals("true"))
  }

  def chanFromSubscriberModel(sid: Int)(jss: JsValue) = {
    Channel.create(sid, forName(jss, "protocol"), forName(jss, "mode"), forName(jss, "description"),
                   forName(jss, "userId"), forName(jss, "password"), forName(jss, "channelUrl"))
  }

  def planFromSubscriberModel(sid: Int)(jss: JsValue) {
    val chan = Channel.findByUrl(forName(jss, "channelUrl")).get
    val plan = Plan.make(sid, chan.id, forName(jss, "name"), forName(jss, "description"), forName(jss, "icon"),
                        forName(jss, "fulfill"), forName(jss, "pick"), forName(jss, "interest"),
                        forName(jss, "template"))
    val schemes = (jss \ "schemes")
    procJsArray(schemes, 0, addSchemeFromSubscriberModel(plan))
  }

  def addSchemeFromSubscriberModel(plan: Plan)(jss: JsValue) {
    Scheme.findByTag(forName(jss, "tag")).foreach(plan.addScheme(_))
  }
}

object jsonHelpers {

  def procJsArray(arr: JsValue, index: Int, func: JsValue => Unit): Unit = {
    arr(index) match {
      case und: JsUndefined => Nil
      case jsval: JsValue => func(jsval); procJsArray(arr, index + 1, func)
    }
  }

  def forName(jsv: JsValue, name: String): String = (jsv \ name).as[String]
  def forNameOption(jsv: JsValue, name: String): Option[String] = (jsv \ name).asOpt[String]
  def forNum(jsv: JsValue, name: String): Int = (jsv \ name).as[Int]
}
