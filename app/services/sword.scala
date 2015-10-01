/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package services

import java.io.ByteArrayInputStream

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.Source
import scala.xml.pull._
import scala.util.{Success, Failure}

import play.api._
import play.api.libs.ws._
import play.api.Play.current
import models.Channel

/** Services for SWORD protocol operations
  * Currently a few SWORD v1.3 client actions
  *
  * @author richardrodgers
  */

  object SwordClient {

    /**
     * Check subscriber SWORD channel info by composing a request for the service
     * document from the server, and if successful attempting to match the
     * configured endpoint, package format, etc. This is implemented as a
     * blocking WS call (not usually recommended) to test response time from
     * the server. Returns status string.
     */
    def checkEndpoint(channel: Channel): String = {

      val endpoint = channel.channelUrl

      def readSwordResponse(response: WSResponse): String = {
        response.status match {
          case 200 =>
            Logger.info("Successful retrieval of service document")
            validateServiceDocument(new XMLEventReader(Source.fromInputStream(
                                    new ByteArrayInputStream(response.body.getBytes))))
          case 401 =>
            Logger.info("Authorization failure")
            s"warning|The request failed authorization on the server: check credentials"
          case _ =>
            Logger.warn("The SWORD server did not accept the request. Response was " + response.toString)
            s"danger|The SWORD server did not accept the request. Response: ${response.toString}"
        }
      }

      def invalid(field: Option[String], value: String): Boolean = {
        field.isEmpty || field.get != value
      }

      def validateServiceDocument(xmlReader: XMLEventReader): String = {
        // we want to validate a few things: server is ver 1.3 of SWORD and
        // collection matches endpoint and accepts application/zip, DSpaceMetsSip packages
        var readingVer = false
        var version: Option[String] = None
        var readingCollection = false
        var readingName = false
        var collectionName: Option[String] = None
        var readingAccept = false
        var accept: Option[String] = None
        var readingPackage = false
        var acceptedPackages: List[String] = List()
        while (xmlReader.hasNext) {
          xmlReader.next match {
            case EvElemStart("sword","version",_,_) => readingVer = true
            case EvElemStart("app","collection",attrs,_) if (attrs("href").text == endpoint) => readingCollection = true
            case EvElemStart("atom","title",_,_) if readingCollection => readingName = true
            case EvElemStart("app","accept",_,_) if readingCollection => readingAccept = true
            case EvElemStart("sword","acceptPackaging",_,_) if readingCollection => readingPackage = true
            case EvText(text) if readingVer => version = Some(text); readingVer = false
            case EvText(text) if readingName => collectionName = Some(text); readingName = false
            case EvText(text) if readingAccept => accept = Some(text); readingAccept = false
            case EvText(text) if readingPackage => acceptedPackages = text :: acceptedPackages; readingPackage = false
            case EvElemEnd("app", "collection") if readingCollection => readingCollection = false
            case _ =>
          }
        }
        // return first validation failure encountered - no real need to pile it on
        if (invalid(version, "1.3"))
          s"danger|Incompatible server SWORD versions: need 1.3 but server supports ${version.get}"
        else if (collectionName.isEmpty)
          s"warning|No SWORD collection exposed at endpoint: '$endpoint' on server"
        else if (invalid(accept, "application/zip"))
          s"danger|Incompatible collection content type: need 'application/zip' but server requires ${accept.get}"
        else if (! acceptedPackages.contains("http://purl.org/net/sword-types/METSDSpaceSIP"))
          s"danger|SWORD collection on server does not support required packaging format: 'http://purl.org/net/sword-types/METSDSpaceSIP'"
        else
          s"success|Destination matches reachable SWORD collection: '${collectionName.get}' on server"
      }

      // construct a serviceDocument URL from channelUrl (using the DSpace URL convention)
      val svcUrl = endpoint.substring(0, endpoint.indexOf("sword") + 5) + "/servicedocument"
      val until = Duration(15000, "millis")
      Logger.info("About to call: " + svcUrl)
      try {
        val resp = Await.ready(WS.url(svcUrl).withAuth(channel.userId, channel.password, WSAuthScheme.BASIC).get(),
                              until)
        resp.value map {
          case Success(response) => readSwordResponse(response)
          case Failure(t) => s"danger|Request for service document failed with message: '${t.getMessage}'"
        } getOrElse("danger|Unknown Error")
      } catch {
        case e: TimeoutException => s"warning|Timed out: no server response in ${until.toSeconds} seconds"
        case e: Throwable => s"danger|Unexpected exception: '${e.getMessage}'"
      }
    }
}
