/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package controllers

import scala.xml.{Attribute, Elem, NodeSeq, Text, Null}

import java.text.SimpleDateFormat
import java.util.{Base64, Date}

import org.joda.time.DateTimeZone
import org.joda.time.format.ISODateTimeFormat

import play.api._
import play.api.Play.current
import play.api.mvc._

import models.{Item, Scheme, Topic}

/**
  * OAIPMH object manages OAI-PMH (Open Archives Initiative - Protocol for Metadata
  * Harvesting) v2.0 protocol operations. The hub is a 'repository' (data provider)
  * in OAI-PMH terms, meaning it responds in the protocol-ordained manner to
  * 'harvester' requests. Data model: OAI Items = Items, and OAI Sets = Topics
  *
  * @author richardrodgers
  */

object OAIPMH extends Controller {

  val recLimit = 100
  val adminEmail = Play.configuration.getString("hub.admin.email").get
  val iso8601 = ISODateTimeFormat.dateTimeNoMillis.withZone(DateTimeZone.UTC)
  val prFormat = new SimpleDateFormat("yyyy-MM-dd")
  val tsFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
  val urlEncoder = Base64.getUrlEncoder
  val urlDecoder = Base64.getUrlDecoder
  lazy val metaSchemeId = Scheme.findByTag("meta").map(_.id).getOrElse(0L)
  lazy val earliestDatestamp = Item.findOldest.map(_.created).getOrElse(new Date).getTime
  lazy val tagCache = Scheme.all.map(sch => sch.id -> sch.tag).toMap

  def provide = Action { implicit request =>
    val query = request.queryString.mapValues(_.head)
    val verb = query.get("verb")
    Ok(
      <OAI-PMH xmlns="http://www.openarchives.org/OAI/2.0/"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://www.openarchives.org/OAI/2.0/
        http://www.openarchives.org/OAI/2.0/OAI-PMH.xsd">
        <responseDate>{iso8601.print(System.currentTimeMillis())}</responseDate>
        { withAttrs(<request>http://{request.host}{request.path}</request>, query) }
        { if (verb.isDefined) processQuery(verb.get, query, request.host, request.path) else error("badVerb", "Missing verb") }
      </OAI-PMH>
    ).as("text/xml")
  }

  private def processQuery(verb: String, query: Map[String, String], host: String, path: String): NodeSeq = {
    try {
      verb match {
        case "GetRecord" => getRecord(query, host)
        case "Identify" => identify(host, path)
        case "ListIdentifiers" => listIdentifiers(query)
        case "ListMetadataFormats" => listMetadataFormats
        case "ListRecords" => listRecords(query, host)
        case "ListSets" => listSets(query)
        case _ => error("badVerb", "Unknown verb: " + verb)
      }
    } catch {
      case ex: IllegalStateException => error("noRecordsMatch")
      case _: Throwable => error("badArgument")
    }
  }

  private def getRecord(query: Map[String, String], host: String) = {
    query.get("identifier").map( identifier =>
      query.get("metadataPrefix").map( metadataPrefix =>
        if ("oai_dc".equals(metadataPrefix))
          Item.findByKey(identifier).map( item =>
            <GetRecord>
             { itemRecord(item, host) }
            </GetRecord>
          ).getOrElse(error("idDoesNotExist", "Not found: " + identifier))
        else error("cannotDisseminateFormat")
      ).getOrElse(error("badArgument", "Missing metadataPrefix"))
    ).getOrElse(error("badArgument", "Missing identifier"))
  }

  private def identify(host: String, path: String) =
    <Identify>
      <repositoryName>SCOAP3 Topic Hub</repositoryName>
      <baseURL>http://{host}{path}</baseURL>
      <protocolVersion>2.0</protocolVersion>
      <adminEmail>{adminEmail}</adminEmail>
      <earliestDatestamp>{iso8601.print(earliestDatestamp)}</earliestDatestamp>
      <deletedRecord>no</deletedRecord>
      <granularity>YYYY-MM-DD</granularity>
    </Identify>

  private def listIdentifiers(query: Map[String, String]) = {
    val hits = itemQuery(query)
    if (hits.isEmpty) throw new IllegalStateException()
    val results = if (hits.length > recLimit) hits.dropRight(1) else hits
    <ListIdentifiers>
      { for (item <- results)
          yield itemHeader(item)
      } {
        if (hits.length > recLimit)
          { withAttrs(<resumptionToken>{results(recLimit-1).created}</resumptionToken>,
                      Map("completeListSize" -> itemCount.toString)) }
        else if (query.get("resumptionToken").isDefined)
          { withAttrs(<resumptionToken/>, Map("completeListSize" -> itemCount.toString)) }
      }
    </ListIdentifiers>
  }

  // For now, only the required 'oai_dc' format supported for all items: no identifier-specific formats
  private def listMetadataFormats =
    <ListMetadataFormats>
      <metadataFormat>
        <metadataPrefix>oai_dc</metadataPrefix>
        <schema>http://www.openarchives.org/OAI/2.0/oai_dc.xsd</schema>
        <metadataNamespace>http://www.openarchives.org/OAI/2.0/oai_dc/</metadataNamespace>
      </metadataFormat>
    </ListMetadataFormats>

  private def listRecords(query: Map[String, String], host: String) = {
    val hits = itemQuery(query)
    if (hits.isEmpty) throw new IllegalStateException()
    val results = if (hits.length > recLimit) hits.dropRight(1) else hits
    <ListRecords>
      { for (item <- results)
          yield itemRecord(item, host)
      } {
        if (hits.length > recLimit)
          { withAttrs(<resumptionToken>{results(recLimit-1).created}</resumptionToken>,
                      Map("completeListSize" -> itemCount.toString)) }
        else if (query.get("resumptionToken").isDefined)
          { withAttrs(<resumptionToken/>, Map("completeListSize" -> itemCount.toString)) }
      }
    </ListRecords>
  }

  private def listSets(query: Map[String, String]) = {
    val token = query.get("resumptionToken")
    val start = token.map(t => tsFormat.parse(t)).getOrElse(new Date(earliestDatestamp - 1000))
    // look-ahead by 1 to see if we need to paginate results
    val hits = Topic.createdAfter(start, recLimit + 1)
    val results = if (hits.length > recLimit) hits.dropRight(1) else hits
    <ListSets>
      { for (topic <- results.filter(_.scheme_id != metaSchemeId)) yield
        <set>
          <setSpec>{tagCache.get(topic.scheme_id).get}:{toSetSpec(topic)}</setSpec>
          <setName>{bestName(topic)}</setName>
        </set>
      } {
        if (hits.length > recLimit)
          { withAttrs(<resumptionToken>{results(recLimit-1).created}</resumptionToken>,
                      Map("completeListSize" -> topicCount.toString)) }
        else if (token.isDefined)
          { withAttrs(<resumptionToken/>, Map("completeListSize" -> topicCount.toString)) }
      }
    </ListSets>
  }

  private def itemRecord(item: Item, host: String) =
    <record>
      { itemHeader(item) }
      <metadata>
        <oai_dc:dc
          xmlns:oai_dc="http://www.openarchives.org/OAI/2.0/oai_dc/"
          xmlns:dc="http://purl.org/dc/elements/1.1/"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
          xsi:schemaLocation="http://www.openarchives.org/OAI/2.0/oai_dc/
          http://www.openarchives.org/OAI/2.0/oai_dc.xsd">
          <dc:title>{item.metadataValue("title")}</dc:title>
          { for (author <- item.metadataValues("author")) yield
             <dc:creator>{author}</dc:creator>
          }
          <dc:identifier>http://{host}/item/{item.id}</dc:identifier>
          <dc:identifier>http://{host}/item/package/{item.id}</dc:identifier>
        </oai_dc:dc>
      </metadata>
    </record>

  private def itemHeader(item: Item) =
    <header>
      <identifier>{item.objKey}</identifier>
      <datestamp>{iso8601.print(item.created.getTime)}</datestamp>
      { for (topic <- item.topics.filter(_.scheme_id != metaSchemeId)) yield
        <setSpec>{tagCache.get(topic.scheme_id).get}:{toSetSpec(topic)}</setSpec>
      }
    </header>

  private def itemQuery(query: Map[String, String]) = {
    val earliest = query.get("from").map(f => prFormat.parse(f)).getOrElse(new Date(earliestDatestamp - 1000))
    val fromToken = query.get("resumptionToken").map(t => tsFormat.parse(t)).getOrElse(earliest)
    val latest = query.get("until").map(u => prFormat.parse(u)).getOrElse(new Date())
    // look-ahead by 1 to see if we need to paginate results
    query.get("set").map( s => {
      val split = s.indexOf(":")
      val decoded = fromSetSpec(s.substring(split + 1))
      Topic.forSchemeAndTag(s.substring(0, split), decoded).map( topic =>
        Item.inTopicRange(topic.id, fromToken, latest, recLimit + 1)
      ).getOrElse(throw new IllegalStateException())
    }).getOrElse(Item.inRange(fromToken, latest, recLimit + 1))
  }

  private def withAttrs(elem: Elem, attrs: Map[String, String]) =
    attrs.foldLeft(elem){ case (el, (key, attr)) => el % Attribute(None, key, Text(attr), Null) }
  private def error(code: String, msg: String = null) = <error code={code}>{msg}</error>
  private def itemCount = Item.createdAfterCount(new Date(earliestDatestamp))
  private def topicCount = Topic.createdAfterCount(new Date(earliestDatestamp))
  private def bestName(topic: Topic) = if ("No Label".equals(topic.name)) topic.tag else topic.name
  private def toSetSpec(topic: Topic) = urlEncoder.encodeToString(topic.tag.getBytes("UTF-8")).replace('=', '~')
  private def fromSetSpec(setSpec: String) = new String(urlDecoder.decode(setSpec.replace('~', '=')), "UTF-8")
}
