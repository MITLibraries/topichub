/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package controllers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.Await

import akka.actor.Props

import java.util.Date

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.concurrent.Akka
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.mvc._
import play.api.Play.current

import models._
import services.contentModelJson._
import services.publisherModelJson._

case class HubContext(user: Option[User])

object Application extends Controller {

  val harvester = Akka.system.actorOf(Props[workers.HarvestWorker], name="harvester")

  def index = Action {
    // Create dummy user if not allready there
    val user = User.findById(1)
    if (user.isEmpty) {
      val dummy = new User(1, "richard", "rrodgers@mit.edu", "pwd", "admin", new Date, new Date) //User.findByName(username).get
      User.create(dummy.name, dummy.email, dummy.password, dummy.role)
    }
    Ok(views.html.scoap3_index(Scheme.withGentype("topic").filter(!_.tag.equals("meta"))))
  }

  def about = Action { implicit request =>
    Ok(views.html.about())
  }

  def workbench = Action { implicit request =>
    Ok(views.html.workbench())
  }

  def item(id: Int) = Action { implicit request =>
    Item.findById(id).map( item =>
      Ok(views.html.item(item))
    ).getOrElse(NotFound(views.html.trouble("No such item: " + id)))
  }

  def itemBrowse(filter: String, id: Int, page: Int) = Action { implicit request =>
    filter match {
      case "collection" => itemBrowseCollection(id, page)
      case "topic" => itemBrowseTopic(id, page)
      case _ => NotFound(views.html.trouble("No such filter: " + filter))
    }
  }

  private def itemBrowseTopic(id: Int, page: Int)(implicit request: Request[AnyContent]): Result = {
    Topic.findById(id).map( topic =>
      Ok(views.html.item_browse(id, topic.pagedItems(page, 10), "topic", topic.name, page, topic.itemCount))
    ).getOrElse(NotFound(views.html.trouble("No such topic: " + id)))
  }

  private def itemBrowseCollection(id: Int, page: Int)(implicit request: Request[AnyContent]): Result = {
    Collection.findById(id).map( coll =>
      Ok(views.html.item_browse(id, Item.inCollection(id, page), "collection", coll.description, page, Item.collectionCount(coll.id)))
    ).getOrElse(NotFound(views.html.trouble("No such collection")))
  }

  def topics = Action { implicit request =>
    Ok(views.html.topic_index(Scheme.withGentype("topic").filter(!_.tag.equals("meta"))))
  }

  def topic(id: Int) = Action { implicit request =>
    Topic.findById(id).map( t =>
      Ok(views.html.topic(t))
    ).getOrElse(NotFound(views.html.trouble("No such topic: " + id)))
  }

  def topicBrowse(scheme_id: Int, page: Int) = Action { implicit request =>
    Scheme.findById(scheme_id).map( scheme =>
      Ok(views.html.topic_browse(scheme.id, Topic.withScheme(scheme.id, page), scheme.description, page, scheme.topicCount))
    ).getOrElse(NotFound(views.html.trouble("No such scheme")))
  }

  def topicValidate(scheme_id: Int) = /*isAuthenticated { username => */  Action { implicit request =>
    Scheme.findById(scheme_id).map( scheme => {
       val topic_id = request.body.asFormUrlEncoded.get.get("topic").get.head
       checkTopic(scheme, topic_id, /*username */ "richard")
    }
    ).getOrElse(NotFound(views.html.trouble("No such scheme")))
  }

  private def checkTopic(scheme: Scheme, tag: String, userName: String)(implicit request: Request[AnyContent]): Result = {
    val topic = Topic.forSchemeAndTag(scheme.tag, tag)
    if (! topic.isEmpty)
      // just redirect to topic page
      Redirect(routes.Application.topic(topic.get.id))
    else {
      // not a known topic in this scheme - attempt validation
      val validator = scheme.validator
      val res = if (! validator.isEmpty) Await.result(validator.get.validate(tag), Duration(10000, "millis")) else Left("No validator found")
      Ok(views.html.topic_validate(scheme, tag, res))
    }
  }

  val pubForm = Form(
  	mapping(
      "id" -> ignored(0),
      "userId" -> ignored(0),
      "tag" -> nonEmptyText,
      "name" -> nonEmptyText,
      "description" -> nonEmptyText,
      "category" -> nonEmptyText,
      "status" -> ignored(""),
      "link" -> optional(text),
      "logo" -> optional(text),
      "created" -> ignored(new Date)
    )(Publisher.apply)(Publisher.unapply)
  )

  def publishers = Action { implicit request =>
  	Ok(views.html.publisher_index())
  }

  def newPublisher =  Action { implicit request => //mustAuthenticate { username => implicit request =>
    Ok(views.html.publisher_create(null, pubForm))
  }

  def publisher(id: Int) = Action { implicit request => {
    val userName = "richard"//session.get("username").getOrElse("")
    Publisher.findById(id).map( pub =>
      Ok(views.html.publisher(pub, User.findByName(userName)))
    ).getOrElse(NotFound(views.html.trouble("No such publisher: " + id)))
    }
  }

  def createPublisher = Action { implicit request => //isAuthenticated { username => implicit request =>
    pubForm.bindFromRequest.fold(
      errors => BadRequest(views.html.publisher_create(null, errors)),
      value => {
        val user = User.findById(1).get
        val pub = Publisher.make(user.id, value.tag, value.name, value.description, value.category, value.status, value.link, value.logo)
        Redirect(routes.Application.editPublisher(pub.id))
      }
    )
  }

  def publisherBrowse(filter: String, value: String, page: Int) = Action { implicit request =>
    filter match {
      case "category" => Ok(views.html.publisher_browse(value, Publisher.inCategory(value, page), value, page, Publisher.categoryCount(value))) //publisherBrowseCategory(value, page)
      case _ => NotFound(views.html.trouble("No such filter"))
    }
  }

  private def ownsPublisher(username: String, pub: Publisher, result: Result)(implicit request: Request[AnyContent]): Result = {
    val user = User.findByName(username).get
    if (user.hasPublisher(pub.id)) {
      result
    } else {
      Unauthorized(views.html.trouble("You are not authorized"))
    }
  }

  def editPublisher(id: Int) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Publisher.findById(id).map( pub =>
      ownsPublisher("richard", pub, Ok(views.html.publisher_edit(pub)))
    ).getOrElse(NotFound(views.html.trouble("No such publisher: " + id)))
  }

  val harvestForm = Form(
    mapping(
      "id" -> ignored(0),
      "publisher_id" -> ignored(1),
      "name" -> nonEmptyText,
      "protocol" -> nonEmptyText,
      "service_url" -> nonEmptyText,
      "resource_url" -> nonEmptyText,
      "freq" -> number,
      "start" -> date,
      "updated" -> ignored(new Date)
    )(Harvest.apply)(Harvest.unapply)
  )

  val startHarvestForm = Form(
    single {
      "span" -> number
    }
  )

  def harvest(id: Int) = Action { implicit request =>
    Harvest.findById(id).map( harvest =>
      Ok(views.html.harvest(harvest, startHarvestForm))
    ).getOrElse(NotFound(views.html.trouble("No such harvest: " + id)))
  }

  def startHarvest(id: Int) = Action { implicit request =>
    Harvest.findById(id).map( harvest =>
      startHarvestForm.bindFromRequest.fold(
        errors => BadRequest(views.html.harvest(harvest, errors)),
        value =>  {
           val harv = harvest.copy(freq = value)
           harvester ! harv
           // optimistically update - so UI will show last harvest date
           harv.complete
           Redirect(routes.Application.harvest(id))
        }
      )
    ).getOrElse(NotFound(views.html.trouble("No such harvest: " + id)))
  }

  def newHarvest(id: Int) = Action { implicit request =>
    Publisher.findById(id).map( pub =>
      ownsPublisher(/*username*/"richard", pub, Ok(views.html.harvest_create(pub, harvestForm)))
    ).getOrElse(NotFound(views.html.trouble("No such publisher: " + id)))
  }

  def createHarvest(id: Int) = Action { implicit request =>
    val pub = Publisher.findById(id).get
    ownsPublisher(/*username*/"richard", pub,
      harvestForm.bindFromRequest.fold(
        errors => BadRequest(views.html.harvest_create(pub, errors)),
        value => {
          val harv = Harvest.make(id, value.name, value.protocol, value.serviceUrl, value.resourceUrl, value.freq, value.start)
          // also create an inbound channel for this collection - currently limited to SWORD
          // RLR TODO - not clear we need to make a channel if content is only harvested
          /*
          val chan = Channel.make("sword", "package", "inbound", pub.pubId + ":" + coll.description + " deposits", "user", "password", "/sword/collection/" + coll.id)
          // make collection the channel owner
          chan.setOwner("coll", coll.id)
          conveyor ! coll
          */
          Redirect(routes.Application.publisher(id))
        }
      )
    )
  }

  def deleteHarvest(id: Int) = Action { implicit request =>
    Harvest.findById(id).map( harvest => {
      Harvest.delete(id)
      Redirect(routes.Application.publisher(harvest.publisher.get.id))
    }
    ).getOrElse(NotFound(views.html.trouble("No such harvest: " + id)))
  }

  def collections = Action { implicit request =>
    Ok(views.html.collection_index())
  }

  val collForm = Form(
    mapping(
      "id" -> ignored(0),
      "publisher_id" -> ignored(1),
      "ctype_id" -> number,
      "resmap_id" -> number,
      "tag" -> nonEmptyText,
      "description" -> nonEmptyText,
      "policy" -> nonEmptyText,
      "created" -> ignored(new Date),
      "updated" -> ignored(new Date),
      "deposits" -> ignored(0)
    )(Collection.apply)(Collection.unapply)
  )

  def newCollection(id: Int) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Publisher.findById(id).map( pub =>
      ownsPublisher(/*username*/"richard", pub, Ok(views.html.collection_create(pub, collForm)))
    ).getOrElse(NotFound(views.html.trouble("No such publisher: " + id)))
  }

  def createCollection(id: Int) = Action { implicit request => //isAuthenticated { username => implicit request =>
    val pub = Publisher.findById(id).get
    ownsPublisher(/*username*/"richard", pub,
      collForm.bindFromRequest.fold(
        errors => BadRequest(views.html.collection_create(pub, errors)),
        value => {
          val coll = Collection.make(id, value.ctypeId, value.resmapId, value.tag, value.description, value.policy)
          // also create an inbound channel for this collection - currently limited to SWORD
          // RLR TODO - not clear we need to make a channel if content is only harvested
          /*
          val chan = Channel.make("sword", "package", "inbound", pub.pubId + ":" + coll.description + " deposits", "user", "password", "/sword/collection/" + coll.id)
          // make collection the channel owner
          chan.setOwner("coll", coll.id)
          conveyor ! coll
          */
          Redirect(routes.Application.publisher(id))
        }
      )
    )
    //).getOrElse(NotFound("No such publisher"))
  }

  val schemeForm = Form(
    mapping(
      "id" -> ignored(0),
      "tag" -> nonEmptyText,
      "gentype" -> nonEmptyText,
      "category" -> nonEmptyText,
      "description" -> nonEmptyText,
      "home" -> optional(text),
      "logo" -> optional(text),
      "created" -> ignored(new Date)
    )(Scheme.apply)(Scheme.unapply)
  )

  def schemes = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(/*username*/"richard", Ok(views.html.scheme_list(Scheme.all)))
    Ok(views.html.scheme_index(Scheme.all))
  }

  def scheme(id: Int) = Action { implicit request =>
    Scheme.findById(id).map( scheme =>
      Ok(views.html.scheme(scheme))
    ).getOrElse(NotFound(views.html.trouble("No such scheme: " + id)))
  }

  def newScheme = Action { implicit request => // isAuthenticated { username => implicit request =>
     //isAnalyst(username, Ok(views.html.new_scheme(schemeForm)))
     Ok(views.html.scheme_create(schemeForm))
  }

  def editScheme(id: Int) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Scheme.findById(id).map( scheme =>
      //isAnalyst(username, Ok(views.html.scheme_edit(scheme)))
      Ok(views.html.scheme_edit(scheme))
    ).getOrElse(NotFound(views.html.trouble("No such scheme: " + id)))
  }

  def createScheme = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username,
      schemeForm.bindFromRequest.fold(
        errors => BadRequest(views.html.scheme_create(errors)),
        value => {
          Scheme.create(value.tag, value.gentype, value.category, value.description, value.link, value.logo)
          Redirect(routes.Application.schemes)
        }
      )
    //)
  }

  // content types
  val ctypeForm = Form(
    mapping(
      "id" -> ignored(0),
      "tag" -> nonEmptyText,
      "label" -> nonEmptyText,
      "description" -> nonEmptyText,
      "logo" -> optional(text)
    )(ContentType.apply)(ContentType.unapply)
  )

  val ctSchemeForm = Form(
    tuple(
      "scheme_id" -> number,
      "foo" -> ignored(0)
    )
  )

  def contentTypes = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username, Ok(views.html.ctype_list(ContentType.all)))
    Ok(views.html.ctype_index(ContentType.all))
  }

  def contentType(id: Int) = Action { implicit request => // isAuthenticated { username => implicit request =>
    ContentType.findById(id).map( ctype =>
      //isAnalyst(username, Ok(views.html.ctype(ctype, ctSchemeForm)))
      Ok(views.html.ctype(ctype, ctSchemeForm))
    ).getOrElse(NotFound(views.html.trouble("No such content type")))
  }

  def newContentType = Action { implicit request => //isAuthenticated { username => implicit request =>
     //isAnalyst(username, Ok(views.html.new_ctype(ctypeForm)))
     Ok(views.html.ctype_create(ctypeForm))
  }

  def createContentType = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username,
      ctypeForm.bindFromRequest.fold(
        errors => BadRequest(views.html.ctype_create(errors)),
        value => {
          ContentType.create(value.tag, value.label, value.description, value.logo)
          Redirect(routes.Application.contentTypes)
        }
      )
    //)
  }

  def addContentTypeScheme(id: Int, relation: String) = Action { implicit request =>
    ctSchemeForm.bindFromRequest.fold(
      errors => {
        val ct = ContentType.findById(id).get
        BadRequest(views.html.ctype(ct, errors))
      },
      value => {
        val ct2 = ContentType.findById(id).get
        val (scheme_id, _) = ctSchemeForm.bindFromRequest.get
        val scheme = Scheme.findById(scheme_id).get
        ct2.addScheme(scheme, relation /*"meta" */)
        Redirect(routes.Application.contentType(id))
      }
    )
  }

  def removeContentTypeScheme(cid: Int, sid: Int, relation: String) = Action { implicit request =>
    ContentType.findById(cid).map( ctype =>
      Scheme.findById(sid).map( scheme => {
        ctype.removeScheme(scheme, relation)
        Redirect(routes.Application.contentType(cid))
      }).getOrElse(NotFound(views.html.trouble("No such scheme")))
    ).getOrElse(NotFound(views.html.trouble("No such content type")))
  }

  // content formats
  val cfmtForm = Form(
    mapping(
      "id" -> ignored(0),
      "tag" -> nonEmptyText,
      "label" -> nonEmptyText,
      "description" -> nonEmptyText,
      "url" -> nonEmptyText,
      "mimetype" -> nonEmptyText,
      "logo" -> optional(text)
    )(ContentFormat.apply)(ContentFormat.unapply)
  )

  def contentFormats = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username, Ok(views.html.ctype_list(ContentType.all)))
    Ok(views.html.cformat_index(ContentFormat.all))
  }

  def contentFormat(id: Int) = Action { implicit request => // isAuthenticated { username => implicit request =>
    ContentFormat.findById(id).map( cfmt =>
      //isAnalyst(username, Ok(views.html.ctype(ctype, ctSchemeForm)))
      Ok(views.html.cformat(cfmt, ctSchemeForm))
    ).getOrElse(NotFound(views.html.trouble("No such content format: " + id)))
  }

  def newContentFormat = Action { implicit request => //isAuthenticated { username => implicit request =>
     //isAnalyst(username, Ok(views.html.new_ctype(ctypeForm)))
     Ok(views.html.cformat_create(cfmtForm))
  }

  def createContentFormat = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username,
      cfmtForm.bindFromRequest.fold(
        errors => BadRequest(views.html.cformat_create(errors)),
        value => {
          ContentFormat.create(value.tag, value.label, value.description, value.url, value.mimetype, value.logo)
          Redirect(routes.Application.contentFormats)
        }
      )
    //)
  }

  // resource maps
  val resmapForm = Form(
    mapping(
      "id" -> ignored(0),
      "tag" -> nonEmptyText,
      "description" -> nonEmptyText,
      "swordurl" -> optional(text)
    )(ResourceMap.apply)(ResourceMap.unapply)
  )

  val rmSchemeForm = Form(
    tuple(
      "scheme_id" -> number,
      "format_id" -> number,
      "source" -> nonEmptyText,
      "rank" -> ignored(0)
    )
  )

  def resourceMaps = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username, Ok(views.html.pkgmap_list(PackageMap.all)))
    Ok(views.html.resmap_index(ResourceMap.all))
  }

  def resourceMap(id: Int) = Action { implicit request => // isAuthenticated { username => implicit request =>
    ResourceMap.findById(id).map( resmap =>
      //isAnalyst(username, Ok(views.html.pkgmap(pmap, pmSchemeForm)))
      Ok(views.html.resmap(resmap, rmSchemeForm))
    ).getOrElse(NotFound(views.html.trouble("No such resource map: " + id)))
  }

  def newResourceMap = Action { implicit request => //isAuthenticated { username => implicit request =>
     //isAnalyst(username, Ok(views.html.new_pkgmap(pkgmapForm)))
     Ok(views.html.resmap_create(resmapForm))
  }

  def createResourceMap = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username,
      resmapForm.bindFromRequest.fold(
      errors => BadRequest(views.html.resmap_create(errors)),
      value => {
        ResourceMap.create(value.tag, value.description, value.swordUrl)
        Redirect(routes.Application.resourceMaps)
      }
    )//)
  }

  def newResourceMapping(id: Int) = Action { implicit request =>
    rmSchemeForm.bindFromRequest.fold(
      errors => {
        val rm = ResourceMap.findById(id).get
        BadRequest(views.html.resmap(rm, errors))
      },
      value => {
        val rm2 = ResourceMap.findById(id).get
        val (scheme_id, format_id, source, _) = rmSchemeForm.bindFromRequest.get
        rm2.addMapping(scheme_id, format_id, source, 0)
        Redirect(routes.Application.resourceMap(id))
      }
    )
  }

  def removeResourceMapping(rid: Int, sid: Int, source: String) = Action { implicit request =>
    ResourceMap.findById(rid).map( resmap =>
      Scheme.findById(sid).map( scheme => {
        resmap.removeMapping(scheme, source)
        Redirect(routes.Application.resourceMap(rid))
      }).getOrElse(NotFound(views.html.trouble("No such scheme: " + sid)))
    ).getOrElse(NotFound(views.html.trouble("No such resource map: " + rid)))
  }

  val finderForm = Form(
    mapping(
      "id" -> ignored(0),
      "scheme_id" -> number,
      "content_format_id" -> number,
      "description" -> nonEmptyText,
      "cardinality" -> nonEmptyText,
      "idKey" -> nonEmptyText,
      "idLabel" -> nonEmptyText,
      "author" -> nonEmptyText,
      "created" -> ignored(new Date)
    )(Finder.apply)(Finder.unapply)
  )

  def finders(tag: String) = Action { implicit request =>
    Scheme.findByTag(tag).map( scheme =>
      Ok(views.html.finder_list(Finder.findByScheme(scheme.id)))
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme: " + tag)))
  }

  def newFinder(tag: String) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Scheme.findByTag(tag).map( scheme =>
      //isAnalyst(username, Ok(views.html.new_finder(tag, finderForm)))
      Ok(views.html.finder_create(tag, finderForm))
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme: " + tag)))
  }

  def createFinder(tag: String) = Action { implicit request =>
    Scheme.findByTag(tag).map( scheme =>
      finderForm.bindFromRequest.fold(
        errors => BadRequest(views.html.finder_create(tag, errors)),
        value => {
          Finder.create(scheme.id, value.formatId, value.description,
                        value.cardinality, value.idKey, value.idLabel, value.author)
          Redirect(routes.Application.finders(tag))
        }
      )
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme: " + tag)))
  }

  def deleteFinder(tag: String, id: Int) = Action { implicit request =>
    Scheme.findByTag(tag).map( scheme => {
      Finder.delete(id)
      Ok(views.html.finder_list(Finder.findByScheme(scheme.id)))
    }
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme: " + tag)))
  }

  val validatorForm = Form(
    mapping(
      "id" -> ignored(0),
      "scheme_id" -> number,
      "description" -> nonEmptyText,
      "userId" -> nonEmptyText,
      "password" -> nonEmptyText,
      "serviceCode" -> nonEmptyText,
      "serviceUrl" -> nonEmptyText,
      "author" -> nonEmptyText,
      "created" -> ignored(new Date)
    )(Validator.apply)(Validator.unapply)
  )

  def newValidator(tag: String) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Scheme.findByTag(tag).map( scheme =>
      //isAnalyst(username, Ok(views.html.new_validator(schemeId, validatorForm)))
      Ok(views.html.validator_create(tag, validatorForm))
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme: " + tag)))
  }

  def createValidator(tag: String) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Scheme.findByTag(tag).map( scheme =>
      validatorForm.bindFromRequest.fold(
        errors => BadRequest(views.html.validator_create(tag, errors)),
        value => {
          Validator.create(scheme.id, value.description, value.userId, value.password, value.serviceCode, value.serviceUrl, value.author)
          Redirect(routes.Application.finders(tag))
        }
      )
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme: " + tag)))
  }

  def deleteValidator(tag: String, id: Int) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Scheme.findByTag(tag).map( scheme => {
      Validator.delete(id)
      Ok(views.html.finder_list(Finder.findByScheme(scheme.id)))
    }
    ).getOrElse(NotFound(views.html.trouble("Unknown scheme: " + tag)))
  }

  val modelForm = Form(
    single(
      "model" -> nonEmptyText
    )
  )

  def newHubModel = Action { implicit request =>
    Ok(views.html.cmodel_create(modelForm))
  }

  def contentModel = Action { implicit request =>
    Ok(jsonContentModel)
  }

  def publisherModel = Action { implicit request =>
    Ok(jsonPublisherModel)
  }

  def addContentModel = Action { implicit request =>
    // read a content model from posted data and update system in cases where
    // model components are not already installed. Note that
    // this relies on the uniqueness of scheme, etc tags across hubs
    modelForm.bindFromRequest.fold(
      errors =>
        BadRequest(views.html.cmodel_create(errors)),
      value => {
        buildContentModel(Json.parse(value))
        Redirect(routes.Application.newHubModel)
      }
    )
  }

  def addPublisherModel = Action { implicit request =>
    // read a content model from posted data and update system in cases where
    // model components are not already installed. Note that
    // this relies on the uniqueness of scheme, etc tags across hubs
    modelForm.bindFromRequest.fold(
      errors =>
      BadRequest(views.html.cmodel_create(errors)),
      value => {
        buildPublisherModel(Json.parse(value))
        Redirect(routes.Application.newHubModel)
      }
    )
  }

  // convenience method for now - refine for real use later
  def purge = Action { implicit request =>
    val now = new Date
    Item.deleteBefore(now)
    Topic.deleteUnlinkedBefore(now)
    Ok("too late to go back now")
  }
}
