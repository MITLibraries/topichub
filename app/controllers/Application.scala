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
import play.api.mvc.Security._
import play.api.Play.current

import models._
import services.contentModelJson._
import services.publisherModelJson._
import services.subscriberModelJson._
import services.Emailer
import workers.Cataloger

case class HubContext(user: Option[User])

object Application extends Controller with Security {

  val harvester = Akka.system.actorOf(Props[workers.HarvestWorker], name="harvester")
  val indexer = Akka.system.actorOf(Props[workers.IndexWorker], name="indexer")
  val conveyor = Akka.system.actorOf(Props[workers.ConveyorWorker], name="conveyor")

  def index = Action {
    Ok(views.html.static.home(Scheme.withGentype("topic").filter(!_.tag.equals("meta"))))
  }

  def about = Action { implicit request =>
    Ok(views.html.static.about())
  }

  def workbench = isAnalyst { identity =>
    implicit request =>
      Ok(views.html.static.workbench())
  }

  val feedbackForm = Form(
    tuple(
      "email" -> email,
      "reply" -> boolean,
      "comment" -> nonEmptyText
    ) verifying("Invalid Email address", result => true)
  )

  def feedback = Action { implicit request =>
    Ok(views.html.static.feedback(feedbackForm))
  }

  def takeFeedback = Action { implicit request =>
    feedbackForm.bindFromRequest.fold(
      errors =>
        BadRequest(views.html.static.feedback(errors)),
      value => {
        Emailer.feedback(value._1, value._3, value._2)
        //val user = User.findByName(session.get("username").get).get
        Redirect(routes.Application.index)
      }
    )
  }

  def topics = Action { implicit request =>
    Ok(views.html.topic.index(Scheme.withGentype("topic").filter(!_.tag.equals("meta"))))
  }

  def topic(id: Int) = Action { implicit request =>
    Topic.findById(id).map( t =>
      Ok(views.html.topic.show(t, Subscriber.findByUserId(1)))
    ).getOrElse(NotFound(views.html.static.trouble("No such topic: " + id)))
  }

  def topicBrowse(scheme_id: Int, page: Int) = Action { implicit request =>
    Scheme.findById(scheme_id).map( scheme =>
      Ok(views.html.topic.browse(scheme.id, Topic.withScheme(scheme.id, page), scheme.description, page, scheme.topicCount))
    ).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + scheme_id)))
  }

  def topicSubscribe(id: Int, cancel: Boolean) = Action { implicit request => //= mustAuthenticate { username => implicit request =>
    Topic.findById(id).map( topic =>
      topicSubIfSubscriber(User.findByName("richard").get, topic, cancel)
    ).getOrElse(NotFound(views.html.static.trouble("No such topic: " + id)))
  }

  private def topicSubIfSubscriber(user: User, topic: Topic, cancel: Boolean)(implicit request: Request[AnyContent]): Result = {
    Subscriber.findByUserId(user.id).map( sub => {
        if (cancel) {
          sub.subscriptionFor(topic.id).map (sc => sc.cancel)
        } else {
          conveyor ! sub.subscribeTo(topic)
        }
        Redirect(routes.Application.topic(topic.id))
      }
    ).getOrElse(Redirect(routes.Application.subscribers))
  }

  def topicValidate(scheme_id: Int) = /*isAuthenticated { username => */  Action { implicit request =>
    Scheme.findById(scheme_id).map( scheme => {
       val topic_id = request.body.asFormUrlEncoded.get.get("topic").get.head
       checkTopic(scheme, topic_id, /*username */ "richard")
    }
    ).getOrElse(NotFound(views.html.static.trouble("No such scheme")))
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
      Ok(views.html.topic.validate(scheme, tag, res))
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
  	Ok(views.html.publisher.index())
  }

  def newPublisher =  isAuthenticated { identity =>
    implicit request =>
    Ok(views.html.publisher.create(null, pubForm))
  }

  def publisher(id: Int) = Action { implicit request => {
    val user = if ( play.api.Play.isTest(play.api.Play.current) ) {
        User.findById(1).get.identity
      } else {
        request.session.get("connected").getOrElse("")
      }

    Publisher.findById(id).map( pub =>
      Ok(views.html.publisher.show(pub, User.findByIdentity(user)))
    ).getOrElse(NotFound(views.html.static.trouble("No such publisher: " + id)))
    }
  }

  def createPublisher = isAuthenticated { identity =>
    implicit request =>
    pubForm.bindFromRequest.fold(
      errors => BadRequest(views.html.publisher.create(null, errors)),
      value => {
        val pub = Publisher.make(identity.id, value.tag, value.name, value.description, value.category, value.status, value.link, value.logo)
        Redirect(routes.Application.editPublisher(pub.id))
      }
    )
  }

  def publisherBrowse(filter: String, value: String, page: Int) = Action { implicit request =>
    filter match {
      case "category" => Ok(views.html.publisher.browse(value, Publisher.inCategory(value, page), value, page, Publisher.categoryCount(value))) //publisherBrowseCategory(value, page)
      case _ => NotFound(views.html.static.trouble("No such filter"))
    }
  }

  private def ownsPublisher(user: User, pub: Publisher, result: Result)(implicit request: Request[AnyContent]): Result = {
    if (user.hasPublisher(pub.id)) {
      result
    } else {
      Unauthorized(views.html.static.trouble("You are not authorized"))
    }
  }

  def editPublisher(id: Int) = isAuthenticated { identity =>
    implicit request =>
    Publisher.findById(id).map( pub =>
      ownsPublisher(identity, pub, Ok(views.html.publisher.edit(pub)))
    ).getOrElse(NotFound(views.html.static.trouble("No such publisher: " + id)))
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
      Ok(views.html.harvest.show(harvest, startHarvestForm))
    ).getOrElse(NotFound(views.html.static.trouble("No such harvest: " + id)))
  }

  def startHarvest(id: Int) = Action { implicit request =>
    Harvest.findById(id).map( harvest =>
      startHarvestForm.bindFromRequest.fold(
        errors => BadRequest(views.html.harvest.show(harvest, errors)),
        value =>  {
           val harv = harvest.copy(freq = value)
           harvester ! harv
           // optimistically update - so UI will show last harvest date
           harv.complete
           Redirect(routes.Application.harvest(id))
        }
      )
    ).getOrElse(NotFound(views.html.static.trouble("No such harvest: " + id)))
  }

  def pullKnownItem(cid: Int, hid: Int, oid: String, force: Boolean) = Action { implicit request =>
    Collection.findById(cid).map ( coll =>
      Harvest.findById(hid).map( harvest => {
        harvester ! (oid, coll, harvest, force)
        Ok(views.html.harvest.index("pulled: " + oid))
      }).getOrElse(NotFound(views.html.static.trouble("No such harvest: " + hid)))
    ).getOrElse(NotFound(views.html.static.trouble("No such collection: " + cid)))
  }

  def newHarvest(id: Int) = Action { implicit request =>
    Publisher.findById(id).map( pub =>
      ownsPublisher(User.findById(1).get, pub, Ok(views.html.harvest.create(pub, harvestForm)))
    ).getOrElse(NotFound(views.html.static.trouble("No such publisher: " + id)))
  }

  def createHarvest(id: Int) = Action { implicit request =>
    val pub = Publisher.findById(id).get
    ownsPublisher(User.findById(1).get, pub,
      harvestForm.bindFromRequest.fold(
        errors => BadRequest(views.html.harvest.create(pub, errors)),
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
    ).getOrElse(NotFound(views.html.static.trouble("No such harvest: " + id)))
  }

  def collections = Action { implicit request =>
    Ok(views.html.collection.index())
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
      ownsPublisher(User.findById(1).get, pub, Ok(views.html.collection.create(pub, collForm)))
    ).getOrElse(NotFound(views.html.static.trouble("No such publisher: " + id)))
  }

  def createCollection(id: Int) = Action { implicit request => //isAuthenticated { username => implicit request =>
    val pub = Publisher.findById(id).get
    ownsPublisher(User.findById(1).get, pub,
      collForm.bindFromRequest.fold(
        errors => BadRequest(views.html.collection.create(pub, errors)),
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
    Ok(views.html.scheme.index(Scheme.all))
  }

  def scheme(id: Int) = Action { implicit request =>
    Scheme.findById(id).map( scheme =>
      Ok(views.html.scheme.show(scheme))
    ).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + id)))
  }

  def newScheme = Action { implicit request => // isAuthenticated { username => implicit request =>
     //isAnalyst(username, Ok(views.html.new_scheme(schemeForm)))
     Ok(views.html.scheme.create(schemeForm))
  }

  def editScheme(id: Int) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Scheme.findById(id).map( scheme =>
      //isAnalyst(username, Ok(views.html.scheme_edit(scheme)))
      Ok(views.html.scheme.edit(scheme))
    ).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + id)))
  }

  def createScheme = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username,
      schemeForm.bindFromRequest.fold(
        errors => BadRequest(views.html.scheme.create(errors)),
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
    Ok(views.html.content_type.index(ContentType.all))
  }

  def contentType(id: Int) = Action { implicit request => // isAuthenticated { username => implicit request =>
    ContentType.findById(id).map( ctype =>
      //isAnalyst(username, Ok(views.html.ctype(ctype, ctSchemeForm)))
      Ok(views.html.content_type.show(ctype, ctSchemeForm))
    ).getOrElse(NotFound(views.html.static.trouble("No such content type")))
  }

  def newContentType = Action { implicit request => //isAuthenticated { username => implicit request =>
     //isAnalyst(username, Ok(views.html.new_ctype(ctypeForm)))
     Ok(views.html.content_type.create(ctypeForm))
  }

  def createContentType = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username,
      ctypeForm.bindFromRequest.fold(
        errors => BadRequest(views.html.content_type.create(errors)),
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
        BadRequest(views.html.content_type.show(ct, errors))
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
      }).getOrElse(NotFound(views.html.static.trouble("No such scheme")))
    ).getOrElse(NotFound(views.html.static.trouble("No such content type")))
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
    Ok(views.html.content_format.index(ContentFormat.all))
  }

  def contentFormat(id: Int) = Action { implicit request => // isAuthenticated { username => implicit request =>
    ContentFormat.findById(id).map( cfmt =>
      //isAnalyst(username, Ok(views.html.ctype(ctype, ctSchemeForm)))
      Ok(views.html.content_format.show(cfmt, ctSchemeForm))
    ).getOrElse(NotFound(views.html.static.trouble("No such content format: " + id)))
  }

  def newContentFormat = Action { implicit request => //isAuthenticated { username => implicit request =>
     //isAnalyst(username, Ok(views.html.new_ctype(ctypeForm)))
     Ok(views.html.content_format.create(cfmtForm))
  }

  def createContentFormat = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username,
      cfmtForm.bindFromRequest.fold(
        errors => BadRequest(views.html.content_format.create(errors)),
        value => {
          ContentFormat.create(value.tag, value.label, value.description, value.url, value.mimetype, value.logo)
          Redirect(routes.Application.contentFormats)
        }
      )
    //)
  }

  // content profiles
  val cprofForm = Form(
    mapping(
      "id" -> ignored(0),
      "tag" -> nonEmptyText,
      "label" -> nonEmptyText,
      "description" -> nonEmptyText
    )(ContentProfile.apply)(ContentProfile.unapply)
  )

  def contentProfiles = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username, Ok(views.html.ctype_list(ContentType.all)))
    Ok(views.html.content_profile.index(ContentProfile.all))
  }

  def contentProfile(id: Int) = Action { implicit request => // isAuthenticated { username => implicit request =>
    ContentProfile.findById(id).map( cprof =>
      //isAnalyst(username, Ok(views.html.ctype(ctype, ctSchemeForm)))
      Ok(views.html.content_profile.show(cprof, ctSchemeForm))
    ).getOrElse(NotFound(views.html.static.trouble("No such content profile: " + id)))
  }

  def newContentProfile = Action { implicit request => //isAuthenticated { username => implicit request =>
     //isAnalyst(username, Ok(views.html.new_ctype(ctypeForm)))
     Ok(views.html.content_profile.create(cprofForm))
  }

  def createContentProfile = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username,
      cprofForm.bindFromRequest.fold(
        errors => BadRequest(views.html.content_profile.create(errors)),
        value => {
          ContentProfile.create(value.tag, value.label, value.description)
          Redirect(routes.Application.contentProfiles)
        }
      )
    //)
  }

  def addContentProfileScheme(id: Int) = Action { implicit request =>
    ctSchemeForm.bindFromRequest.fold(
      errors => {
        val cp = ContentProfile.findById(id).get
        BadRequest(views.html.content_profile.show(cp, errors))
      },
      value => {
        val cp2 = ContentProfile.findById(id).get
        val (scheme_id, _) = ctSchemeForm.bindFromRequest.get
        val scheme = Scheme.findById(scheme_id).get
        cp2.addScheme(scheme)
        Redirect(routes.Application.contentProfile(id))
      }
    )
  }

  def removeContentProfileScheme(pid: Int, sid: Int) = Action { implicit request =>
    ContentProfile.findById(pid).map( cprof =>
      Scheme.findById(sid).map( scheme => {
        cprof.removeScheme(scheme)
        Redirect(routes.Application.contentProfile(pid))
      }).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + sid)))
    ).getOrElse(NotFound(views.html.static.trouble("No such content profile: " + pid)))
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
    Ok(views.html.resource_map.index(ResourceMap.all))
  }

  def resourceMap(id: Int) = Action { implicit request => // isAuthenticated { username => implicit request =>
    ResourceMap.findById(id).map( resmap =>
      //isAnalyst(username, Ok(views.html.pkgmap(pmap, pmSchemeForm)))
      Ok(views.html.resource_map.show(resmap, rmSchemeForm))
    ).getOrElse(NotFound(views.html.static.trouble("No such resource map: " + id)))
  }

  def newResourceMap = Action { implicit request => //isAuthenticated { username => implicit request =>
     //isAnalyst(username, Ok(views.html.new_pkgmap(pkgmapForm)))
     Ok(views.html.resource_map.create(resmapForm))
  }

  def createResourceMap = Action { implicit request => //isAuthenticated { username => implicit request =>
    //isAnalyst(username,
      resmapForm.bindFromRequest.fold(
      errors => BadRequest(views.html.resource_map.create(errors)),
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
        BadRequest(views.html.resource_map.show(rm, errors))
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
      }).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + sid)))
    ).getOrElse(NotFound(views.html.static.trouble("No such resource map: " + rid)))
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
      Ok(views.html.finder.index(Finder.findByScheme(scheme.id)))
    ).getOrElse(NotFound(views.html.static.trouble("Unknown scheme: " + tag)))
  }

  def newFinder(tag: String) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Scheme.findByTag(tag).map( scheme =>
      //isAnalyst(username, Ok(views.html.new_finder(tag, finderForm)))
      Ok(views.html.finder.create(tag, finderForm))
    ).getOrElse(NotFound(views.html.static.trouble("Unknown scheme: " + tag)))
  }

  def createFinder(tag: String) = Action { implicit request =>
    Scheme.findByTag(tag).map( scheme =>
      finderForm.bindFromRequest.fold(
        errors => BadRequest(views.html.finder.create(tag, errors)),
        value => {
          Finder.create(scheme.id, value.formatId, value.description,
                        value.cardinality, value.idKey, value.idLabel, value.author)
          Redirect(routes.Application.finders(tag))
        }
      )
    ).getOrElse(NotFound(views.html.static.trouble("Unknown scheme: " + tag)))
  }

  def deleteFinder(tag: String, id: Int) = Action { implicit request =>
    Scheme.findByTag(tag).map( scheme => {
      Finder.delete(id)
      Ok(views.html.finder.index(Finder.findByScheme(scheme.id)))
    }
    ).getOrElse(NotFound(views.html.static.trouble("Unknown scheme: " + tag)))
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
      Ok(views.html.validator.create(tag, validatorForm))
    ).getOrElse(NotFound(views.html.static.trouble("Unknown scheme: " + tag)))
  }

  def createValidator(tag: String) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Scheme.findByTag(tag).map( scheme =>
      validatorForm.bindFromRequest.fold(
        errors => BadRequest(views.html.validator.create(tag, errors)),
        value => {
          Validator.create(scheme.id, value.description, value.userId, value.password, value.serviceCode, value.serviceUrl, value.author)
          Redirect(routes.Application.finders(tag))
        }
      )
    ).getOrElse(NotFound(views.html.static.trouble("Unknown scheme: " + tag)))
  }

  def deleteValidator(tag: String, id: Int) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Scheme.findByTag(tag).map( scheme => {
      Validator.delete(id)
      Ok(views.html.finder.index(Finder.findByScheme(scheme.id)))
    }
    ).getOrElse(NotFound(views.html.static.trouble("Unknown scheme: " + tag)))
  }

  val subscriberForm = Form(
    mapping(
      "id" -> ignored(0),
      "user_id" -> ignored(0),
      "name" -> nonEmptyText,
      "category" -> nonEmptyText,
      "contact" -> nonEmptyText,
      "link" -> optional(text),
      "logo" -> optional(text),
      "created" -> ignored(new Date)
    )(Subscriber.apply)(Subscriber.unapply)
  )

  def subscribers = Action { implicit request =>
    Ok(views.html.subscriber.index())
  }

  def subscriber(id: Int) = Action { implicit request => {
    val userName = "richard"//session.get("username").getOrElse("")
    Subscriber.findById(id).map( sub =>
      Ok(views.html.subscriber.show(sub, User.findByName(userName)))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
    }
  }

  def newSubscriber = Action { implicit request =>//mustAuthenticate { username => implicit request =>
    Ok(views.html.subscriber.create(subscriberForm))
  }

  def createSubscriber = Action { implicit request => //isAuthenticated { username => implicit request =>
    subscriberForm.bindFromRequest.fold(
      errors => BadRequest(views.html.subscriber.create(errors)),
      value => {
        val user = User.findById(1).get
        val sub = Subscriber.make(user.id, value.name, value.category, value.contact, value.link, value.logo)
        Redirect(routes.Application.editSubscriber(sub.id))
      }
    )
  }

  private def ownsSubscriber(username: String, sub: Subscriber, result: Result)(implicit request: Request[AnyContent]): Result = {
    val user = User.findByName(username).get
    //if (user.hasPublisher(pub.id)) {
      result
    //} else {
    //  Unauthorized(views.html.static.trouble("You are not authorized"))
    //}
  }

  def editSubscriber(id: Int) = Action { implicit request => //isAuthenticated { username => implicit request =>
    Subscriber.findById(id).map( sub =>
      ownsSubscriber("richard", sub, Ok(views.html.subscriber.edit(sub, interestForm)))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
  }

  def subscriberBrowse(filter: String, value: String, page: Int) = Action { implicit request =>
    filter match {
      case "category" => Ok(views.html.subscriber.browse(value, Subscriber.inCategory(value, page), value, page, Subscriber.categoryCount(value))) //publisherBrowseCategory(value, page)
      case _ => NotFound(views.html.static.trouble("No such filter"))
    }
  }

  val channelForm = Form(
    mapping(
      "id" -> ignored(0),
      "subscriberId" -> ignored(0),
      "protocol" -> nonEmptyText,
      "mode" -> nonEmptyText,
      "description" -> nonEmptyText,
      "userId" -> nonEmptyText,
      "password" -> nonEmptyText,
      "channelUrl" -> nonEmptyText,
      "created" -> ignored(new Date),
      "updated" -> ignored(new Date),
      "transfers" -> ignored(0)
    )(Channel.apply)(Channel.unapply)
  )

  def channel(id: Int) = Action { implicit request => // isAuthenticated { username => implicit request =>
    Channel.findById(id).map( chan =>
      Ok(views.html.channel.show(chan))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber destination: " + id)))
  }

  def newChannel(sid: Int) = Action { implicit request => // isAuthenticated { username => implicit request =>
    //ownsSubscriber(username, sid, Ok(views.html.new_channel(sid, channelForm)))
    Ok(views.html.channel.create(sid, channelForm))
  }

  def createChannel(sid: Int) = Action { implicit request => //isAuthenticated { username => implicit request =>
    //ownsSubscriber(username, sid, channelForm.bindFromRequest.fold (
    channelForm.bindFromRequest.fold (
      errors => BadRequest(views.html.channel.create(sid, errors)),
      value => {
        val chan = Channel.make(sid, value.protocol, value.mode, value.description, value.userId, value.password, value.channelUrl)
        Redirect(routes.Application.subscriber(sid))
      }
    )
    //)
  }

  def subscriberDashboard = isAuthenticated { identity =>
    implicit request =>
    println(identity)
    val sub = Subscriber.findByUserId(identity.id)
    if (sub == None) {
      NotFound(views.html.static.trouble("No Subscriber found for your User Account"))
    } else {
      Ok(views.html.subscriber.dashboard(sub.get))
    }
  }

  val interestForm = Form(
    single(
      "scheme_id" -> number
    )
  )

  def addSubscriberInterest(id: Int, action: String) = Action { implicit request =>
    interestForm.bindFromRequest.fold(
      errors => {
        val sub = Subscriber.findById(id).get
        BadRequest(views.html.subscriber.edit(sub, errors))
      },
      value => {
        val sub2 = Subscriber.findById(id).get
        val schemeId = interestForm.bindFromRequest.get
        val scheme = Scheme.findById(schemeId).get
        sub2.addInterest(scheme, action)
        Redirect(routes.Application.editSubscriber(id))
      }
    )
  }

  def removeSubscriberInterest(id: Int, sid: Int) = Action { implicit request =>
    Subscriber.findById(id).map( sub =>
      Scheme.findById(sid).map( scheme => {
        sub.removeInterest(scheme)
        Redirect(routes.Application.editSubscriber(sub.id))
      }).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + sid)))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
  }

  def subscriptionBrowse(filter: String, value: Int, page: Int) = Action { implicit request =>
    filter match {
      case "scheme" => Ok(views.html.subscription.browse(1, Subscription.inScheme(1, value, page), filter, value, page, Subscription.schemeCount(1, value)))
      case _ => NotFound(views.html.static.trouble("No such filter: " + filter))
    }
  }

  def holdBrowse(id: Int, page: Int) = Action { implicit request =>
    Subscriber.findById(id).map( sub =>
      Ok(views.html.hold.browse(sub.id, sub.holds(page), page, sub.holdCount))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
  }

  def resolveHold(id: Int, accept: Boolean) = Action { implicit request =>
    Hold.findById(id).map( hold => {
      conveyor ! (hold, accept)
      Redirect(routes.Application.holdBrowse(1, 0))
    }
    ).getOrElse(NotFound(views.html.static.trouble("No such hold: " + id)))
  }

  def pickBrowse(id: Int, page: Int) = Action { implicit request =>
    Subscriber.findById(id).map( sub =>
      Ok(views.html.topic_pick.browse(sub.id, sub.picks(page), page, sub.pickCount))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
  }

  def resolvePick(id: Int, accept: Boolean) = Action { implicit request =>
    TopicPick.findById(id).map( pick => {
      conveyor ! (pick, accept)
      Redirect(routes.Application.pickBrowse(1, 0))
    }
    ).getOrElse(NotFound(views.html.static.trouble("No such topic pick: " + id)))
  }

  val modelForm = Form(
    single(
      "model" -> nonEmptyText
    )
  )

  def newHubModel = Action { implicit request =>
    Ok(views.html.utils.cmodel_create(modelForm))
  }

  def contentModel = Action { implicit request =>
    Ok(jsonContentModel)
  }

  def publisherModel = Action { implicit request =>
    Ok(jsonPublisherModel)
  }

  def subscriberModel = Action { implicit request =>
    Ok(jsonSubscriberModel)
  }

  def addContentModel = Action { implicit request =>
    // read a content model from posted data and update system in cases where
    // model components are not already installed. Note that
    // this relies on the uniqueness of scheme, etc tags across hubs
    modelForm.bindFromRequest.fold(
      errors =>
        BadRequest(views.html.utils.cmodel_create(errors)),
      value => {
        buildContentModel(Json.parse(value))
        Redirect(routes.Application.newHubModel)
      }
    )
  }

  def addPublisherModel = Action { implicit request =>
    // read a publisher model from posted data and update system in cases where
    // model components are not already installed. Note that
    // this relies on the uniqueness of scheme, etc tags across hubs
    modelForm.bindFromRequest.fold(
      errors =>
      BadRequest(views.html.utils.cmodel_create(errors)),
      value => {
        buildPublisherModel(Json.parse(value))
        Redirect(routes.Application.newHubModel)
      }
    )
  }

  def addSubscriberModel = Action { implicit request =>
    // read a subscriber model from posted data and update system in cases where
    // model components are not already installed. Note that
    // this relies on the uniqueness of scheme, etc tags across hubs
    modelForm.bindFromRequest.fold(
      errors =>
      BadRequest(views.html.utils.cmodel_create(errors)),
      value => {
        buildSubscriberModel(Json.parse(value))
        Redirect(routes.Application.newHubModel)
      }
    )
  }

  def reindex(dtype: String) = Action { implicit request =>
    indexer ! dtype
    Ok("Reindexing " + dtype + "s")
  }

  val sandboxForm = Form(
    tuple(
      "expression" -> nonEmptyText,
      "resourceUrl" -> nonEmptyText
    )
  )

  // sandbox for testing finder logic
  def sandbox = Action { implicit request =>
    Ok(views.html.static.sandbox(sandboxForm, List("<empty>")))
  }

  def testExpression = Action { implicit request =>
    sandboxForm.bindFromRequest.fold(
      errors => BadRequest(views.html.static.sandbox(errors, List("<error>"))),
      value => {
        val results = Cataloger.testExpression(value._1, "XPath", value._2)
        val filledForm = sandboxForm.bindFromRequest
        Ok(views.html.static.sandbox(filledForm, results))
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
