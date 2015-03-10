/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import scala.collection.Set
import scala.collection.mutable.HashSet
import scala.xml.{Elem, Node, NodeSeq, XML}
import scala.util.matching.Regex
import scala.xml.factory.XMLLoader

import akka.actor.Actor

import java.io.{File, InputStream}
import java.util.Date

import javax.xml.parsers.SAXParser

import org.xml.sax.InputSource

import scales.xml.jaxen._
import scales.xml._
import scales.utils._
import ScalesUtils._
import ScalesXml._

import services.{Store, StoredContent}
import models.{Collection, ContentType, Finder, Item, ResourceMap, Scheme, Topic}

class CatalogWorker extends Actor {
  def receive = {
    case item: Item => Cataloger.catalog(item)
    case _ => println("bar")
  }
}

class Cataloger(resmap: ResourceMap, content: StoredContent) {
  // cache documents - so they won't be re-loaded & re-parsed
  var docCache = Map[String, Doc]()
  // cache finder values - to avoid reprocessing of documents
  var infoCache = Map[String, Seq[String]]()
  // count of added topics
  var addedTopics = 0

  def topicsOld(scheme: Scheme, item: Item) = {
    // where in the item package is data for this scheme?
    val (source, format, rank) = resmap.mappingsForScheme(scheme).head
    val (idHits, lblHits) = Finder.forSchemeAndFormat(scheme.id, format) match {
      case Some(x) => process(source, x)
      case None => (List(), List())
    }
    // add cardinality checking here
    var idx = 0
    println("IDHits size: " + idHits.size)
    for (id <- idHits) {
      // check for and utilize existing topics
      var tp: Topic = Topic.forSchemeAndTag(scheme.tag, id) match {
        case Some(x) => x
        case _ => createTopic(scheme, id, lblHits(idx)) //Topic.create(scheme.id, id, lblHits(idx)); Topic.forSchemeAndId(scheme.schemeId, id).get
      }
      item.addTopic(tp)
      addedTopics += 1
      idx += 1
    }
  }

  def topics(scheme: Scheme, item: Item) = {
    // where in the item's resource map is data for this scheme? Skip if unmapped
    resmap.mappingsForScheme(scheme).map { case (source, format, rank) =>
      val (idHits, lblHits) = Finder.forSchemeAndFormat(scheme.id, format) match {
        case Some(x) => process(source, x)
        case None => (List(), List())
      }
      // add cardinality checking here
      var idx = 0
      println("IDHits size: " + idHits.size)
      for (id <- idHits) {
        // check for and utilize existing topics
        var tp: Topic = Topic.forSchemeAndTag(scheme.tag, id) match {
          case Some(x) => x
          case _ => createTopic(scheme, id, lblHits(idx)) //Topic.create(scheme.id, id, lblHits(idx)); Topic.forSchemeAndId(scheme.schemeId, id).get
        }
        if (! item.hasTopic(tp)) {
          item.addTopic(tp)
          addedTopics += 1
        }
        idx += 1
      }
    }
  }

  private def process(source: String, finder: Finder) = {
    val doc = docToParse(source)
    var idHits: Seq[String]= null
    var lblHits: Seq[String] = null
    if (doc != null) {
      // do Id & label
      println("in process about to evaluate: " + finder.idKey)
      var keyParts = finder.idKey.split(" ")
      // new way
      println("keyParts0: " + keyParts(0))
      val xp = new ScalesXPath(keyParts(0)).withNameConversion(ScalesXPath.localOnly)
      val hits = xp.evaluate(top(doc))
      println("Post eval num hits: " + hits.size)
      if (keyParts.length == 2) {
        val regX = keyParts(1).r
        val theHits = hits map ( h =>
          h match {
            case Left(x) => val regX(l) = x.attribute.value; l
            //case Right(x) => regX findFirstIn x.firstChild.get.item().value
            case Right(x) => val regX(m) = concatText(x); m
            //case Right(x) => val regX(m) = x.firstChild.get.item().value; m
          }
        )
        //idHits = theHits.flatten.toSeq
        idHits = theHits.toSeq
      } else {
        val theHits2 = hits map ( h =>
          h match {
            case Left(x) => x.attribute.value
            case Right(x) => concatText(x)
            // case Right(x) => x.firstChild.get.item().value
          }
        )
        idHits = theHits2.toSeq
      }
    }
  /*
  idHits = XPath.evaluate(keyParts(0), doc)
  if (keyParts.length == 2) {
  // hits need to be transformed by Regex
  var Transformer = new Regex(keyParts(1))
  var transList = List[String]()
  for (hit <- idHits) {
  var Transformer(x) = hit
  //var result:String = x
  transList = x :: transList
}
var revList = transList.reverse
idHits = revList
*/
// also stow in infoCache
  idHits.foreach(println)
//infoCache += ("id" -> idHits)
//}
  val idl = finder.idLabel
// if idl is an XPath, evaluate it
  if (idl != null && idl.length > 0 && idl.indexOf("/") >= 0) {
    //val xpl = new ScalesXPath(idl, Map())
    lblHits = xpathFind(idl, source)
    //lblHits =  XPath.evaluate(idl, doc)
  } else if (idl != null && idl.length > 0) {
    println("process filtered value; " + filteredValue(idl, 0))
    var lblList = List[String]()
    var count = 0
    for (a <- idHits) {
      lblList = filteredValue(idl, count) :: lblList
      count += 1
    }
    //var revLblList = lblList.reverse
    lblHits = lblList.reverse
  } else
    lblHits = List("No label")
   //}
   // equalize length if needed
    if (idHits.length > lblHits.length) {
      (idHits, lblHits.padTo(idHits.length, "No Label"))
    } else (idHits, lblHits)
  }

  private def concatText(node: XmlPath) = {
    node.foldLeft("")(_+_.item().value)
  }

  def createTopic(scheme: Scheme, tag: String, title: String): Topic = {
    Topic.create(scheme.id, tag, title)
    val topic = Topic.forSchemeAndTag(scheme.tag, tag).get
    Indexer.index(topic)
    topic
  }

  def metadata(scheme: Scheme, item: Item) {
    // get Finders for this scheme and format
    val label = scheme.tag
    val (source, format, rank) = resmap.mappingsForScheme(scheme).head
    val (idHits, _) = Finder.forSchemeAndFormat(scheme.id, format) match {
      case Some(x) => process(source, x)
      case None => lacksFinder(source, label, item)
    }
    idHits foreach (item.addMetadata(label, _))
  }

  private def lacksFinder(source: String, label: String, item: Item) = {
    // special/degenerate case of using the name of the source
    // as the value itself, rather than finding it *in* the source
    val location = item.location
    if (location.startsWith("remote:")) {
      (List(location.substring(7) + source), List())
    } else {
      (List("Unknown " + label), List())
    }
  }

  def xpathFind(expr: String, source: String) = {
    val xp = new ScalesXPath(expr, Map())
    val hits = xp.evaluate(top(docToParse(source)))
    val theHits = hits map ( h =>
      h match {
        case Left(x) => x.attribute.value
        case Right(x) => concatText(x)
      }
    )
    //theHits.flatten.toSeq
    theHits.toSeq
  }

  def filteredValue(token: String, index: Int): String = {
    val start = token.indexOf("{")
    if (start >= 0) {
      val end = token.indexOf("}", start + 1)
      token.substring(0, start) + filter(token.substring(start + 1, end), index) + filteredValue(token.substring(end + 1), index)
    } else token
  }

  def filter(token: String, index: Int) = {
    // is value cached?
    var value = infoCache.get(token) match {
      case Some(x) =>
      println("In filter token: " + token + " index: " + index + " size: " + x.size)
      x(index)
      case _ => null
    }
    if (value == null) {
      val scheme = Scheme.findByTag(token).get
      if (scheme != null) {
        val (source, format, rank) = resmap.mappingsForScheme(scheme).head
        val finder = Finder.forSchemeAndFormat(scheme.id, format) match {
          case Some(x) => x
          case None => null
        }
        if (finder != null) {
          val (idHits, _) = process(source, finder)
          //val doc =  docToParse(zip, finder.source)
          //if (doc != null) {
          // value = find(doc, finder.idKey)
          if (idHits.size > 0) {
            value = idHits(index)
            var valList: List[String] = List()
            for (idHit <- idHits) {
              println("idHit: " + idHit)
              valList = idHit :: valList
            }
            infoCache += (token -> valList.reverse)
          } else
          value = "Unknown value"
        }
      }
    }
    value
  }

  def docToParse(name: String) = {
    val fname = filteredValue(name, 0)
    println("doc2p: fname: " + fname)
    // check doc cache first
    docCache.get(fname) match {
      case Some(x) => x
      case _ => val doc = stupidLoad(content.resource(fname)); docCache += (fname -> doc); doc
      //case _ => val doc = loadXml(new InputSource(content.resource(fname))); docCache += (fname -> doc); doc
      // sigh - resorting to unbelievably stupid parsing work-around. Currently cannot determine how to configure
      // scales xml loader to ignore DTD, so doing a scala xml load, then converting
    }
  }

  def stupidLoad(in: InputStream) = {
    val scalaDoc = LooseXml.load(in)
    convertFromScalaXml(scalaDoc)
  }
}

object LooseXml extends XMLLoader[Elem] {
  override def parser: SAXParser = {
    val f = javax.xml.parsers.SAXParserFactory.newInstance()
    f.setNamespaceAware(false)
    //f.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
    f.setFeature("http://xml.org/sax/features/validation", false);
    f.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
    f.newSAXParser()
  }
}

object Cataloger {

  def catalog(item: Item) = {
    val coll = Collection.findById(item.collectionId).get
    val resmap = ResourceMap.findById(coll.resmapId).get
    val cataloger = new Cataloger(resmap, Store.content(item))
    val rmap = ResourceMap.findById(coll.resmapId).get
    // start with metadata schemes
    rmap.schemes("meta").foreach( sch => {
      println("Found scheme:" + sch.tag)
      cataloger.metadata(sch, item) }
    )
    // next topic schemes
    rmap.schemes("topic").foreach( cataloger.topics(_, item) )
    // now assign to meta-topics as appropriate
    if (cataloger.addedTopics == 0) {
      // assign to 'null' meta-topic
      item.addTopic(Topic.forSchemeAndTag("meta", "null") match {
        case Some(x) => x
        case _ => makeTopic("meta", "null", "Items lacking any topic");
      })
      println("No topics")
    } else {
      // assign to the catch-all meta-topic as well
      item.addTopic(Topic.forSchemeAndTag("meta", "any") match {
        case Some(x) => x
        case _ => makeTopic("meta", "any", "Items with some topics");
      })
      println("Found some topics")
    }
    // next indexing schemes (which will have already been found as metadata)
    // must follow topic extraction, since items' topics are indexed
    Indexer.index(item)
    // finally, are there any subscriptions to fulfill?
    Conveyor.newItem(item)
    //item.changeState("cataloged")
  }

  /* obselete
  def contentInfo(item: Item) = {
    val coll = Collection.findById(item.collectionId).get
    val resmap = ResourceMap.findById(coll.resmapId).get
    val cataloger = new Cataloger(resmap, Store.content(item))
    // OK look in map for name of content file in package
    val scheme = Scheme.findByTag("meta").get
    val fileInfo = resmap.mappingsForScheme(scheme).head
    val filteredName = cataloger.filteredValue(fileInfo._1, 0)
    (filteredName, fileInfo._2)
  }
  */

  def testFinder(item: Item, source: String, finder: Finder) = {
    val coll = Collection.findById(item.collectionId).get
    val resmap = ResourceMap.findById(coll.resmapId).get
    val cataloger = new Cataloger(resmap, Store.content(item))
    cataloger.process(source, finder)
  }

  def makeTopic(schemeTag: String, topicTag: String, title: String): Topic = {
    val scheme = Scheme.findByTag(schemeTag).get
    Topic.create(scheme.id, topicTag, title);
    Topic.forSchemeAndTag(schemeTag, topicTag).get
  }
}
