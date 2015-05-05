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

  def index = Action { implicit request =>
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
      Ok(views.html.topic.show(t, Subscriber.findById(currentSubscriberId)))
    ).getOrElse(NotFound(views.html.static.trouble("No such topic: " + id)))
  }

  def topicBrowse(scheme_id: Int, page: Int) = Action { implicit request =>
    Scheme.findById(scheme_id).map( scheme =>
      Ok(views.html.topic.browse(scheme.id, Topic.withScheme(scheme.id, page), scheme.description, page, scheme.topicCount))
    ).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + scheme_id)))
  }

  def topicSubscribe(id: Int, cancel: Boolean) = isAuthenticated { identity => implicit request =>
    Topic.findById(id).map( topic =>
      topicSubIfSubscriber(identity, topic, cancel)
    ).getOrElse(NotFound(views.html.static.trouble("No such topic: " + id)))
  }

  private def topicSubIfSubscriber(user: User, topic: Topic, cancel: Boolean)(implicit request: Request[AnyContent]): Result = {
    Subscriber.findById(currentSubscriberId).map( sub => {
        if (cancel) {
          sub.subscriptionFor(topic.id).map(sc => { sc.cancel; sc.unlinkInterest } )
          // remove backing interest if non-template
          sub.interestWithValue(topic.scheme.get.tag, topic.tag).map(i => Interest.delete(i.id))
        } else {
          conveyor ! sub.subscribeTo(topic)
        }
        Redirect(routes.Application.topic(topic.id))
      }
    ).getOrElse(Redirect(routes.Application.subscribers))
  }

  // todo: determine appropriate role (analyst?)
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
    Publisher.findById(id).map( pub =>
      Ok(views.html.publisher.show(pub, User.findByIdentity(getCurrentIdentity(request))))
    ).getOrElse(NotFound(views.html.static.trouble("No such publisher: " + id)))
    }
  }

  def getCurrentIdentity(implicit request: play.api.mvc.RequestHeader) = {
    // This method is useful to get the identity of the current request user for methods
    // that don't explicitly need a signed in user (i.e. it's okay to be anonymous, but
    // if the user is signed in we want to know who they are for some reason).
    request.session.get("connected").getOrElse("")
  }

  // Used in views to allow conditional display of content based on role
  def currentUserHasRole(role: String)(implicit request: play.api.mvc.RequestHeader) = {
    if (getCurrentIdentity != "") {
      User.findByIdentity(getCurrentIdentity).map( u =>
        u.hasRole(role)
      ).getOrElse(false)
    } else {
      false
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

  def harvest(id: Int) = isAuthenticated { identity =>
    implicit request =>
    Harvest.findById(id).map( harvest =>
      ownsPublisher(identity, harvest.publisher.get,
        Ok(views.html.harvest.show(harvest, startHarvestForm)))
    ).getOrElse(NotFound(views.html.static.trouble("No such harvest: " + id)))
  }

  def startHarvest(id: Int) = isAuthenticated { identity =>
    implicit request =>
    Harvest.findById(id).map( harvest =>
      if (harvest.publisher.get.userId == identity.id) {
        startHarvestForm.bindFromRequest.fold(
          errors => BadRequest(views.html.harvest.show(harvest, errors)),
          value =>  {
             val harv = harvest.copy(freq = value)
             harvester ! harv
             // optimistically update - so UI will show last harvest date
             harv.complete
             Redirect(routes.Application.harvest(id))
          }
        )} else {
          Unauthorized(views.html.static.trouble("You are not authorized"))
        }
    ).getOrElse(NotFound(views.html.static.trouble("No such harvest: " + id)))
  }

  def pullKnownItem(cid: Int, hid: Int, oid: String, force: Boolean) =
    isAuthenticated { identity => implicit request =>
    Collection.findById(cid).map ( coll =>
      Harvest.findById(hid).map( harvest => {
        if (harvest.publisher.get.userId == identity.id) {
          harvester ! (oid, coll, harvest, force)
          Ok(views.html.harvest.index("pulled: " + oid))
        } else {
          Unauthorized(views.html.static.trouble("You are not authorized"))
        }
      }).getOrElse(NotFound(views.html.static.trouble("No such harvest: " + hid)))
    ).getOrElse(NotFound(views.html.static.trouble("No such collection: " + cid)))
  }

  def newHarvest(id: Int) = isAuthenticated { identity =>
    implicit request =>
    Publisher.findById(id).map( pub =>
      ownsPublisher(identity, pub, Ok(views.html.harvest.create(pub, harvestForm)))
    ).getOrElse(NotFound(views.html.static.trouble("No such publisher: " + id)))
  }

  def createHarvest(id: Int) = isAuthenticated { identity => implicit request =>
    val pub = Publisher.findById(id).get
    ownsPublisher(identity, pub,
      harvestForm.bindFromRequest.fold(
        errors => BadRequest(views.html.harvest.create(pub, errors)),
        value => {
          Harvest.make(id, value.name, value.protocol, value.serviceUrl,
                       value.resourceUrl, value.freq, value.start)
          Redirect(routes.Application.publisher(id))
        }
      )
    )
  }

  def deleteHarvest(id: Int) = isAuthenticated { identity =>
    implicit request =>
    Harvest.findById(id).map( harvest => {
      if (harvest.publisher.get.userId == identity.id) {
        Harvest.delete(id)
        Redirect(routes.Application.publisher(harvest.publisher.get.id))
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
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

  def newCollection(id: Int) = isAuthenticated { identity => implicit request =>
    Publisher.findById(id).map( pub =>
      ownsPublisher(identity, pub, Ok(views.html.collection.create(pub, collForm)))
    ).getOrElse(NotFound(views.html.static.trouble("No such publisher: " + id)))
  }

  def createCollection(id: Int) = isAuthenticated { identity => implicit request =>
    val pub = Publisher.findById(id).get
    ownsPublisher(identity, pub,
      collForm.bindFromRequest.fold(
        errors => BadRequest(views.html.collection.create(pub, errors)),
        value => {
          val coll = Collection.make(id, value.ctypeId, value.resmapId, value.tag, value.description, value.policy)
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

  def schemes = Action { implicit request =>
    val s = if(currentUserHasRole("analyst")) {
      Scheme.all
    } else {
      Scheme.all.filter(!_.tag.equals("meta"))
    }
    Ok(views.html.scheme.index(s))
  }

  def scheme(id: Int) = Action { implicit request =>
    Scheme.findById(id).map( scheme =>
      Ok(views.html.scheme.show(scheme, Subscriber.findById(currentSubscriberId)))
    ).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + id)))
  }

  def newScheme = isAnalyst { identity => implicit request =>
    Ok(views.html.scheme.create(schemeForm))
  }

  def editScheme(id: Int) = isAnalyst { identity => implicit request =>
    Scheme.findById(id).map( scheme =>
      Ok(views.html.scheme.edit(scheme))
    ).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + id)))
  }

  def createScheme = isAnalyst { identity => implicit request =>
    schemeForm.bindFromRequest.fold(
      errors => BadRequest(views.html.scheme.create(errors)),
      value => {
        Scheme.create(value.tag, value.gentype, value.category, value.description, value.link, value.logo)
        Redirect(routes.Application.schemes)
      }
    )
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

  def contentTypes = isAnalyst { identity => implicit request =>
    Ok(views.html.content_type.index(ContentType.all))
  }

  def contentType(id: Int) = isAnalyst { identity => implicit request =>
    ContentType.findById(id).map( ctype =>
      Ok(views.html.content_type.show(ctype, ctSchemeForm))
    ).getOrElse(NotFound(views.html.static.trouble("No such content type")))
  }

  def newContentType = isAnalyst { identity => implicit request =>
    Ok(views.html.content_type.create(ctypeForm))
  }

  def createContentType = isAnalyst { identity => implicit request =>
    ctypeForm.bindFromRequest.fold(
      errors => BadRequest(views.html.content_type.create(errors)),
      value => {
        ContentType.create(value.tag, value.label, value.description, value.logo)
        Redirect(routes.Application.contentTypes)
      }
    )
  }

  def addContentTypeScheme(id: Int, relation: String) = isAnalyst { identity => implicit request =>
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

  def removeContentTypeScheme(cid: Int, sid: Int, relation: String) = isAnalyst {
    identity => implicit request =>
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

  def contentFormats = isAnalyst { identity => implicit request =>
    Ok(views.html.content_format.index(ContentFormat.all))
  }

  def contentFormat(id: Int) = isAnalyst { identity => implicit request =>
    ContentFormat.findById(id).map( cfmt =>
      Ok(views.html.content_format.show(cfmt, ctSchemeForm))
    ).getOrElse(NotFound(views.html.static.trouble("No such content format: " + id)))
  }

  def newContentFormat = isAnalyst { identity => implicit request =>
    Ok(views.html.content_format.create(cfmtForm))
  }

  def createContentFormat = isAnalyst { identity => implicit request =>
    cfmtForm.bindFromRequest.fold(
      errors => BadRequest(views.html.content_format.create(errors)),
      value => {
        ContentFormat.create(value.tag, value.label, value.description, value.url, value.mimetype, value.logo)
        Redirect(routes.Application.contentFormats)
      }
    )
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

  def resourceMaps = isAnalyst { identity => implicit request =>
    Ok(views.html.resource_map.index(ResourceMap.all))
  }

  def resourceMap(id: Int) = isAnalyst { identity => implicit request =>
    ResourceMap.findById(id).map( resmap =>
      Ok(views.html.resource_map.show(resmap, rmSchemeForm))
    ).getOrElse(NotFound(views.html.static.trouble("No such resource map: " + id)))
  }

  def newResourceMap = isAnalyst { identity => implicit request =>
    Ok(views.html.resource_map.create(resmapForm))
  }

  def createResourceMap = isAnalyst { identity => implicit request =>
    resmapForm.bindFromRequest.fold(
      errors => BadRequest(views.html.resource_map.create(errors)),
      value => {
        ResourceMap.create(value.tag, value.description, value.swordUrl)
        Redirect(routes.Application.resourceMaps)
      }
    )
  }

  def newResourceMapping(id: Int) = isAnalyst { identity => implicit request =>
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

  def removeResourceMapping(rid: Int, sid: Int, source: String) = isAnalyst {
    identity => implicit request =>
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

  def finders(tag: String) = isAnalyst { identity => implicit request =>
    Scheme.findByTag(tag).map( scheme =>
      Ok(views.html.finder.index(Finder.findByScheme(scheme.id)))
    ).getOrElse(NotFound(views.html.static.trouble("Unknown scheme: " + tag)))
  }

  def newFinder(tag: String) = isAnalyst { identity => implicit request =>
    Scheme.findByTag(tag).map( scheme =>
      Ok(views.html.finder.create(tag, finderForm))
    ).getOrElse(NotFound(views.html.static.trouble("Unknown scheme: " + tag)))
  }

  def createFinder(tag: String) = isAnalyst { identity => implicit request =>
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

  def deleteFinder(tag: String, id: Int) = isAnalyst { identity => implicit request =>
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

  def newValidator(tag: String) = isAnalyst { identity => implicit request =>
    Scheme.findByTag(tag).map( scheme =>
      Ok(views.html.validator.create(tag, validatorForm))
    ).getOrElse(NotFound(views.html.static.trouble("Unknown scheme: " + tag)))
  }

  def createValidator(tag: String) = isAnalyst { identity => implicit request =>
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

  def deleteValidator(tag: String, id: Int) = isAnalyst { identity => implicit request =>
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
    val user = if (getCurrentIdentity(request) == "") {
                  None
                } else {
                  User.findByIdentity(getCurrentIdentity(request))
                }
    Subscriber.findById(id).map( sub =>
      Ok(views.html.subscriber.show(sub, user))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
    }
  }

  def newSubscriber = isAuthenticated { identity => implicit request =>
    Ok(views.html.subscriber.create(subscriberForm))
  }

  def createSubscriber = isAuthenticated { identity => implicit request =>
    subscriberForm.bindFromRequest.fold(
      errors => BadRequest(views.html.subscriber.create(errors)),
      value => {
        val sub = Subscriber.make(identity.id, value.name, value.category,
                                  value.contact, value.link, value.logo)
        Redirect(routes.Application.editSubscriber(sub.id))
      }
    )
  }

  private def ownsSubscriber(user: User, sub: Subscriber, result: Result)(implicit request: Request[AnyContent]): Result = {
    if (sub.userId == user.id) {
      result
    } else {
      Unauthorized(views.html.static.trouble("You are not authorized"))
    }
  }

  def editSubscriber(id: Int) = isAuthenticated { identity => implicit request =>
    Subscriber.findById(id).map( sub =>
      ownsSubscriber(identity, sub, Ok(views.html.subscriber.edit(sub, planAddForm)))
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

  def channel(id: Int) = isAuthenticated { identity => implicit request =>
    Channel.findById(id).map( chan =>
      ownsSubscriber(identity, chan.subscriber, Ok(views.html.channel.show(chan)))
      ).getOrElse(NotFound(views.html.static.trouble("No such subscriber destination: " + id)))
  }

  def newChannel(sid: Int) = isAuthenticated { identity => implicit request =>
    ownsSubscriber(identity, Subscriber.findById(sid).get, Ok(views.html.channel.create(sid, channelForm)))
  }

  def createChannel(sid: Int) = isAuthenticated { identity => implicit request =>
    ownsSubscriber(identity, Subscriber.findById(sid).get,
      channelForm.bindFromRequest.fold (
        errors => BadRequest(views.html.channel.create(sid, errors)),
        value => {
          val chan = Channel.make(sid, value.protocol, value.mode, value.description, value.userId, value.password, value.channelUrl)
          Redirect(routes.Application.subscriber(sid))
        }
      )
    )
  }

  def currentSubscriberId(implicit request: play.api.mvc.RequestHeader) = {
    request.session.get("subscriber").getOrElse("0").toInt
  }

  def subscriberDashboard = isAuthenticated { identity =>
    implicit request =>
    val sub = Subscriber.findById(currentSubscriberId)
    if (sub == None) {
      NotFound(views.html.static.trouble("No Subscriber found for your User Account"))
    } else {
      Ok(views.html.subscriber.dashboard(sub.get))
    }
  }

  val planForm = Form(
    mapping(
      "id" -> ignored(0),
      "subscriberId" -> ignored(0),
      "channelId" -> number,
      "name" -> nonEmptyText,
      "description" -> nonEmptyText,
      "icon" -> nonEmptyText,
      "fulfill" -> nonEmptyText,
      "pick" -> nonEmptyText,
      "interest" -> nonEmptyText,
      "template" -> nonEmptyText,
      "created" -> ignored(new Date)
    )(Plan.apply)(Plan.unapply)
  )

  val planAddForm = Form(
    single(
      "scheme_id" -> number
    )
  )

  def plan(id: Int) = isAuthenticated { identity => implicit request =>
    Plan.findById(id).map( plan =>
      ownsSubscriber(identity, plan.subscriber.get, Ok(views.html.plan.show(plan)))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber plan: " + id)))
  }

  def newPlan(sid: Int) = isAuthenticated { identity => implicit request =>
    ownsSubscriber(identity, Subscriber.findById(sid).get,
                   Ok(views.html.plan.create(sid, planForm)))
  }

  def createPlan(sid: Int) = isAuthenticated { identity => implicit request =>
    ownsSubscriber(identity, Subscriber.findById(sid).get, planForm.bindFromRequest.fold (
        errors => BadRequest(views.html.plan.create(sid, errors)),
        value => {
          Plan.make(sid, value.channelId, value.name, value.description, value.icon, value.fulfill, value.pick, value.interest, value.template)
          Redirect(routes.Application.subscriber(sid))
        }
      )
    )
  }

  def addPlanScheme(id: Int) = isAuthenticated { identity => implicit request =>
    Plan.findById(id).map( plan => {
      if (plan.subscriber.get.userId == identity.id) {
        planAddForm.bindFromRequest.fold (
          errors => BadRequest(views.html.subscriber.edit(plan.subscriber.get, errors)),
          value => {
            Scheme.findById(value).map( scheme => {
              plan.addScheme(scheme)
              Redirect(routes.Application.editSubscriber(plan.subscriber.get.id))
            }).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + value)))
          }
        )
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }).getOrElse(NotFound(views.html.static.trouble("No such subscriber plan: " + id)))
  }

  def removePlanScheme(id: Int, schemeId: Int) = isAuthenticated { identity => implicit request =>
    Plan.findById(id).map( plan => {
      if (plan.subscriber.get.userId == identity.id) {
        Scheme.findById(schemeId).map( scheme => {
          plan.removeScheme(scheme)
          Redirect(routes.Application.editSubscriber(plan.subscriber.get.id))
        }).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + schemeId)))
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }).getOrElse(NotFound(views.html.static.trouble("No such subscriber plan: " + id)))
  }

  def deletePlan(id: Int) = isAuthenticated { identity => implicit request =>
    Plan.findById(id).map( plan => {
      if (plan.subscriber.get.userId == identity.id) {
        Plan.delete(id)
        Redirect(routes.Application.subscriber(plan.subscriberId))
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber plan: " + id)))
  }

  def addSubscriberInterest(id: Int, schemeTag: String) = Action { implicit request =>
    Subscriber.findById(id).map( sub =>
      Scheme.findByTag(schemeTag).map( scheme => {
        val intValue = request.body.asFormUrlEncoded.get.get("interest").get.head
        val template = request.body.asFormUrlEncoded.get.get("template").get.head
        conveyor ! sub.addInterest(scheme, intValue, template.equals("true"))
        Redirect(routes.Application.interestBrowse("scheme", scheme.tag))
      }).getOrElse(NotFound(views.html.static.trouble("No such scheme tag: " + schemeTag)))
      ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
  }

  def removeSubscriberInterest(sid: Int, iid: Int) = Action { implicit request =>
    Subscriber.findById(sid).map( sub =>
      Interest.findById(iid).map( interest => {
        sub.removeInterest(interest.scheme.get, interest.intValue)
        Redirect(routes.Application.interestBrowse("scheme", interest.schemeTag))
      }).getOrElse(NotFound(views.html.static.trouble("No such interest: " + iid)))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + sid)))
  }

  def interestBrowse(filter: String, value: String, page: Int) = Action { implicit request =>
    val subId = currentSubscriberId
    filter match {
      case "scheme" => Ok(views.html.interest.browse(subId, Interest.inScheme(subId, value, page), filter, value, page, Interest.schemeCount(subId, value)))
      case "plan" => Ok(views.html.interest.browse(subId, Interest.inPlan(subId, value.toInt, page), filter, value, page, Interest.planCount(subId, value.toInt)))
      case "match" => Ok(views.html.interest.browse(subId, Interest.inMatch(subId, value, page), filter, value, page, Interest.matchCount(subId, value)))
      case _ => NotFound(views.html.static.trouble("No such filter: " + filter))
    }
  }

  def subscriptionBrowse(filter: String, value: Int, page: Int) = isAuthenticated {
      identity => implicit request =>

    val sub = Subscriber.findById(currentSubscriberId)
    if (sub == None) {
      Unauthorized(views.html.static.trouble("You are not authorized"))
    } else {
      filter match {
        case "scheme" => Ok(views.html.subscription.browse(
                            Subscription.inScheme(sub.get.id, value, page),
                            filter, value, page,
                            Subscription.schemeCount(sub.get.id, value)))
        case _ => NotFound(views.html.static.trouble("No such filter: " + filter))
      }
    }
  }

  def holdBrowse(id: Int, page: Int) = isAuthenticated { identity => implicit request =>
    Subscriber.findById(id).map( sub =>
      ownsSubscriber(identity, sub,
                    Ok(views.html.hold.browse(sub.id, sub.holds(page), page, sub.holdCount)))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
  }

  def resolveHold(id: Int, accept: Boolean) = isAuthenticated { identity => implicit request =>
    Hold.findById(id).map( hold => {
      val sub = Subscriber.findById(hold.subscriberId).get
      if (sub.userId == identity.id) {
        conveyor ! (hold, accept)
        Redirect(routes.Application.holdBrowse(sub.id, 0))
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }
    ).getOrElse(NotFound(views.html.static.trouble("No such hold: " + id)))
  }

  def pickBrowse(id: Int, page: Int) = isAuthenticated { identity => implicit request =>
    Subscriber.findById(id).map( sub =>
      ownsSubscriber(identity, sub,
        Ok(views.html.topic_pick.browse(sub.id, sub.picks(page), page, sub.pickCount)))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
  }

  def resolvePick(id: Int, accept: Boolean) = isAuthenticated { identity => implicit request =>
    TopicPick.findById(id).map( pick => {
      val sub = Subscriber.findById(pick.subscriberId).get
      if (sub.userId == identity.id) {
        conveyor ! (pick, accept)
        Redirect(routes.Application.pickBrowse(sub.id, 0))
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }
    ).getOrElse(NotFound(views.html.static.trouble("No such topic pick: " + id)))
  }

  val modelForm = Form(
    single(
      "model" -> nonEmptyText
    )
  )

  def newHubModel = isAdmin { identity => implicit request =>
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

  def addContentModel = isAdmin { identity => implicit request =>
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

  def addPublisherModel = isAdmin { identity => implicit request =>
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

  def addSubscriberModel = isAdmin { identity => implicit request =>
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

  def reindex(dtype: String) = isAdmin { identity => implicit request =>
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
  def sandbox = isAnalyst { identity => implicit request =>
    Ok(views.html.static.sandbox(sandboxForm, List("<empty>")))
  }

  def testExpression = isAnalyst { identity => implicit request =>
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
  def purge = isAdmin { identity => implicit request =>
    val now = new Date
    Item.deleteBefore(now)
    Topic.deleteUnlinkedBefore(now)
    Ok("too late to go back now")
  }
}
