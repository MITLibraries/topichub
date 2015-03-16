package controllers

import play.api.mvc._
import play.api._
import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits._
import play.utils.UriEncoding
import models.HubUtils._
import models._
import play.api.libs.iteratee.Enumerator
import workers.Packager
import java.io.{ByteArrayOutputStream, File, FileInputStream, FileOutputStream, InputStream}

object ItemController extends Controller {

  def item(id: Int) = Action { implicit request =>
    Item.findById(id).map( item =>
      Ok(views.html.item.show(item, Subscriber.findByUserId(1)))
    ).getOrElse(NotFound(views.html.static.trouble("No such item: " + id)))
  }

  def itemBrowse(filter: String, id: Int, page: Int) = Action { implicit request =>
    filter match {
      case "collection" => itemBrowseCollection(id, page)
      case "topic" => itemBrowseTopic(id, page)
      case _ => NotFound(views.html.static.trouble("No such filter: " + filter))
    }
  }

  private def itemBrowseTopic(id: Int, page: Int)(implicit request: Request[AnyContent]): Result = {
    Topic.findById(id).map( topic =>
      Ok(views.html.item.browse(id, topic.pagedItems(page, 10), "topic", topic.name, page, topic.itemCount))
    ).getOrElse(NotFound(views.html.static.trouble("No such topic: " + id)))
  }

  private def itemBrowseCollection(id: Int, page: Int)(implicit request: Request[AnyContent]): Result = {
    Collection.findById(id).map( coll =>
      Ok(views.html.item.browse(id, Item.inCollection(id, page), "collection", coll.description, page, Item.collectionCount(coll.id)))
    ).getOrElse(NotFound(views.html.static.trouble("No such collection")))
  }

  def itemMets(id: Int) = Action { implicit request =>
    Item.findById(id).map( item =>
      Ok(item.toMets)
    ).getOrElse(NotFound(views.html.static.trouble("No such item: " + id)))
  }

  def itemPackage(id: Int) = Action { implicit request =>
    Item.findById(id).map( item =>
      Result(
        header = ResponseHeader(200, Map(CONTENT_TYPE -> "application/zip")),
        body = Enumerator.fromStream(Packager.packageItem(item))
      )
    ).getOrElse(NotFound(views.html.static.trouble("No such item: " + id)))
  }

  def itemDeposit(id: Int) = Action { implicit request =>
    val channel = Subscriber.findByUserId(1).get.channels.headOption.getOrElse {
      //todo handle this more elegantly!
      throw new RuntimeException("You must define a Channel")
    }

    var req = WS.url(channel.channelUrl)
      .withHeaders(CONTENT_TYPE -> "application/zip",
                 "X-packaging" -> "http://purl.org/net/sword-types/METSDSpaceSIP")
      .withAuth(channel.userId, channel.password, WSAuthScheme.BASIC)

    println(req)

    Item.findById(id).map( item =>
      {
        val content = Packager.packageItem(item)
        val bytesOut = new ByteArrayOutputStream
        val buf = new Array[Byte](2048)
        var read = content.read(buf)
        while(read != -1) {
          bytesOut.write(buf, 0, read)
          read = content.read(buf)
        }
        var resp = req.post(bytesOut.toByteArray)
      }
    ).getOrElse(NotFound(views.html.static.trouble("No such item: " + id)))

    Ok("That may have worked.")
  }
}
