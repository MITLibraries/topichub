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
import java.time.Instant
import java.time.temporal.ChronoUnit

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.concurrent.Akka
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.mvc._
import play.api.mvc.Security._
import play.api.Play.current
import play.api.i18n.Messages.Implicits._

import models._
import services.contentModelJson._
import services.publisherModelJson._
import services.subscriberModelJson._
import services.{Emailer, SwordClient}
import workers.Cataloger

case class HubContext(user: Option[User])

object Application extends Controller with Security {

  val harvester = Akka.system.actorOf(Props[workers.HarvestWorker], name="harvester")
  val reaper = Akka.system.actorOf(Props[workers.ReaperWorker], name="reaper")
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
          sub.interestWithValue(topic.scheme.tag, topic.tag).map(i => Interest.delete(i.id))
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

  def currentUser(implicit request: play.api.mvc.RequestHeader) = {
    if (getCurrentIdentity != "") {
      User.findByIdentity(getCurrentIdentity).getOrElse("")
    } else {
      ""
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
      "protocol" -> default(nonEmptyText, "oai-pmh"),
      "service_url" -> nonEmptyText,
      "resource_url" -> nonEmptyText,
      "freq" -> default(number, 1),
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

  def startAllHarvests(key: String) = Action { implicit request =>
    val authorized_key = Play.configuration.getString("auth.harvest.key").get
    val yesterday = Instant.now.truncatedTo(ChronoUnit.DAYS).minus(1, ChronoUnit.DAYS)
    if (key == authorized_key) {
      Harvest.all.map{ h =>
        if(h.updated.before(Date.from(yesterday))) {
          harvester ! h
          h.complete
        } else {
          Logger.info("A harvest tried to start that had an invalid date.")
        }
      }
      Ok("kicked off all harvests")
    } else {
      Unauthorized(views.html.static.trouble("You are not authorized"))
    }
  }

  def pullKnownItem(cid: Int, hid: Int, oid: String, force: Boolean) =
    isAuthenticated { identity => implicit request =>
    Collection.findById(cid).map ( coll =>
      Harvest.findById(hid).map( harvest => {
        if (harvest.publisher.get.userId == identity.id) {
          harvester ! (oid, coll, harvest, force)
          Redirect(routes.Application.index).flashing(
            "success" -> s"pulled: ${oid}")
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

  val cullForm = Form(
    mapping(
      "id" -> ignored(0),
      "publisher_id" -> ignored(1),
      "name" -> nonEmptyText,
      "policy" -> default(nonEmptyText, "soft"),
      "notify_url" -> optional(text),
      "freq" -> default(number, 1),
      "start" -> date,
      "updated" -> ignored(new Date)
    )(Cull.apply)(Cull.unapply)
  )

  val startCullForm = Form(
    single {
      "span" -> number
    }
  )

  def cull(id: Int) = isAuthenticated { identity =>
    implicit request =>
    Cull.findById(id).map( cull =>
      ownsPublisher(identity, cull.publisher.get,
        Ok(views.html.cull.show(cull, startCullForm)))
    ).getOrElse(NotFound(views.html.static.trouble("No such cull: " + id)))
  }

  def startCull(id: Int) = isAuthenticated { identity =>
    implicit request =>
    Cull.findById(id).map( cull =>
      if (cull.publisher.get.userId == identity.id) {
        startCullForm.bindFromRequest.fold(
          errors => BadRequest(views.html.cull.show(cull, errors)),
          value =>  {
             val cl = cull.copy(freq = value)
             reaper ! cl
             // optimistically update - so UI will show last harvest date
             cl.complete
             Redirect(routes.Application.cull(id))
          }
        )} else {
          Unauthorized(views.html.static.trouble("You are not authorized"))
        }
    ).getOrElse(NotFound(views.html.static.trouble("No such cull: " + id)))
  }

  def startAllCulls(key: String) = Action { implicit request =>
    val authorized_key = Play.configuration.getString("auth.harvest.key").get
    if (key == authorized_key) {
      Cull.all.map{ cull =>
        reaper ! cull
        cull.complete }
      Ok("kicked off all culls")
    } else {
      Unauthorized(views.html.static.trouble("You are not authorized"))
    }
  }

  def expungeKnownItem(cid: Int, oid: String, policy: String) =
    isAuthenticated { identity => implicit request =>
    Collection.findById(cid).map ( coll => {
      if (coll.publisher.userId == identity.id) {
        reaper ! (oid, policy)
        Ok("expunged: " + oid)
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }).getOrElse(NotFound(views.html.static.trouble("No such collection: " + cid)))
  }

  def newCull(id: Int) = isAuthenticated { identity =>
    implicit request =>
    Publisher.findById(id).map( pub =>
      ownsPublisher(identity, pub, Ok(views.html.cull.create(pub, cullForm)))
    ).getOrElse(NotFound(views.html.static.trouble("No such publisher: " + id)))
  }

  def createCull(id: Int) = isAuthenticated { identity => implicit request =>
    val pub = Publisher.findById(id).get
    ownsPublisher(identity, pub,
      cullForm.bindFromRequest.fold(
        errors => BadRequest(views.html.cull.create(pub, errors)),
        value => {
          Cull.make(id, value.name, value.policy, value.notifyUrl,
                   value.freq, value.start)
          Redirect(routes.Application.publisher(id))
        }
      )
    )
  }

  def deleteCull(id: Int) = isAuthenticated { identity =>
    implicit request =>
    Cull.findById(id).map( cull => {
      if (cull.publisher.get.userId == identity.id) {
        Cull.delete(id)
        Redirect(routes.Application.publisher(cull.publisher.get.id))
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }
    ).getOrElse(NotFound(views.html.static.trouble("No such cull: " + id)))
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
      "deposits" -> ignored(0),
      "active" -> boolean
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
          val coll = Collection.make(id, value.ctypeId, value.resmapId, value.tag, value.description, value.policy, value.active)
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
        // onboard subscriber by creating a 'review' starter plan, attaching all
        // schemes to it, with a 'drain' (null) channel, since hub has no real
        // delivery information yet, but wants to allow subscriber to operate
        val drain = Channel.make(sub.id, "drain", "none", "No destination", sub.name, "none", "http://example.com")
        val plan = Plan.make(sub.id, drain.id, "Review", "Queue everything for review",
                            "inbox", "review", "review", "review", "review")
        Scheme.withGentype("topic").filter(_.tag != "meta").foreach(plan.addScheme(_))
        Redirect(routes.Application.subscriberDashboard).
                 withSession("connected" -> identity.identity,
                 "subscriber" -> sub.id.toString).flashing(
                 "success" -> "Session Created")
      }
    )
  }

  val joinSubscriberForm = Form(
    single(
      "subscriberid" -> number
    )
  )

  def joinSubscriber = isAuthenticated { identity => implicit request =>
    val subs = Subscriber.all.filterNot(_.userList(true).contains(identity))
    Ok(views.html.subscriber.join(subs, joinSubscriberForm))
  }

  def joinRequest = isAuthenticated { identity => implicit request =>
    joinSubscriberForm.bindFromRequest.fold(
      errors => BadRequest(views.html.subscriber.join(Subscriber.all, errors)),
      value => {
        val subscriber = Subscriber.findById(value).get
        subscriber.linkUser(identity.id)
        val adminEmails = subscriber.adminList.map {user => user.email}.mkString(",")
        val subject = s"${HubUtils.siteName} Request to Join Subscriber"
        val msg = views.txt.email.subscriber_join_request(subscriber, identity).body
        Emailer.subscriberEmails(adminEmails, subject, msg)
        Ok(views.html.subscriber.request())
      }
    )
  }

  def subscriberUsers(id: Int) = isAuthenticated { identity => implicit request =>
    val sub = Subscriber.findById(id).get
    subscriberMember(identity, sub, Ok(views.html.subscriber.users(sub)))
  }

  def subscriberResolveUser(id: Int, userid: Int, res: String) = isAuthenticated { identity => implicit request =>
    val sub = Subscriber.findById(id).get
    val subject = s"${HubUtils.siteName} Request to Join Subscriber ${res}"
    val user = User.findById(userid).get
    val msg = views.txt.email.subscriber_resolve(sub, res).body

    if (sub.adminList.contains(identity)) {
      if (res == "approved") {
        sub.approveUser(userid)
      } else {
        sub.denyUser(userid)
      }
      Emailer.subscriberEmails(user.email, subject, msg)
      Redirect(routes.Application.subscriberUsers(sub.id)).flashing(
        "success" -> s"User was ${res} membership to this Subscriber Group."
      )
    } else {
      Unauthorized(views.html.static.trouble("You are not authorized"))
    }
  }

  def subscriberRemoveUser(id: Int, user: Int) = isAuthenticated { identity => implicit request =>
    val sub = Subscriber.findById(id).get
    if (sub.adminList.contains(identity)) {
      sub.unlinkUser(user)
      Redirect(routes.Application.subscriberUsers(sub.id)).flashing(
        "success" -> "User was removed from this Subscriber Group."
      )
    } else {
      Unauthorized(views.html.static.trouble("You are not authorized"))
    }
  }

  def subscriberToggleAdmin(id: Int, user: Int) = isAuthenticated { identity => implicit request =>
    val sub = Subscriber.findById(id).get
    if (sub.adminList.contains(identity)) {
      if (sub.adminList.contains(User.findById(user).getOrElse(""))) {
        sub.removeAdmin(user)
      } else {
        sub.makeAdmin(user)
      }
      Redirect(routes.Application.subscriberUsers(sub.id)).flashing(
        "success" -> "User admin status was updated for this Subscriber Group."
      )
    } else {
      Unauthorized(views.html.static.trouble("You are not authorized"))
    }
  }

  private def subscriberMember(user: User, sub: Subscriber, result: Result)(implicit request: Request[AnyContent]): Result = {
    if (sub.userList().contains(user)) {
      result
    } else {
      Unauthorized(views.html.static.trouble("You are not authorized"))
    }
  }

  def editSubscriber(id: Int) = isAuthenticated { identity => implicit request =>
    Subscriber.findById(id).map( sub =>
      subscriberMember(identity, sub, Ok(views.html.subscriber.edit(sub, planAddForm)))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
  }

  def subscriberBrowse(filter: String, value: String, page: Int) = Action { implicit request =>
    filter match {
      case "category" => Ok(views.html.subscriber.browse(value, Subscriber.inCategory(value, page), value, page, Subscriber.categoryCount(value))) //publisherBrowseCategory(value, page)
      case _ => NotFound(views.html.static.trouble("No such filter"))
    }
  }

  def updateSessionSubscriber(id: Int) = isAuthenticated { identity => implicit request =>
    subscriberMember(identity, Subscriber.findById(id).get, Redirect(routes.Application.userDashboard).
      withSession("connected" -> identity.identity,
                  "subscriber" -> Subscriber.findById(id).get.id.toString).flashing(
                  "success" -> "Session Subscriber Updated"))
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
      subscriberMember(identity, chan.subscriber, Ok(views.html.channel.show(chan)))
      ).getOrElse(NotFound(views.html.static.trouble("No such subscriber destination: " + id)))
  }

  def checkChannel(id: Int) = isAuthenticated { identity => implicit request =>
    Channel.findById(id).map( chan =>
      subscriberMember(identity, chan.subscriber, { val msg = SwordClient.checkEndpoint(chan).split('|');
                                     Redirect(routes.Application.channel(chan.id)).flashing(
                                     msg(0) -> msg(1)) })
      ).getOrElse(NotFound(views.html.static.trouble("No such subscriber destination: " + id)))
  }

  def newChannel(sid: Int) = isAuthenticated { identity => implicit request =>
    subscriberMember(identity, Subscriber.findById(sid).get, Ok(views.html.channel.create(sid, channelForm)))
  }

  def createChannel(sid: Int) = isAuthenticated { identity => implicit request =>
    subscriberMember(identity, Subscriber.findById(sid).get,
      channelForm.bindFromRequest.fold (
        errors => BadRequest(views.html.channel.create(sid, errors)),
        value => {
          val sub = Subscriber.findById(sid).get
          val oldChans = sub.channels
          val chan = Channel.make(sid, value.protocol, value.mode, value.description, value.userId, value.password, value.channelUrl)
          // continue subscriber onboarding process if this is the first channel
          // subscriber created: hub now knows enough to deliver content,
          // so create a 'deliver' starter plan, set both the review and deliver
          // plans to this new channel, and ditch the drain channel
          if (oldChans.size == 1 && oldChans.head.protocol == "drain") {
            Plan.create(sub.id, chan.id, "Deliver", "Send everything directly",
                        "thumbs-up", "deliver", "review", "review", "review")
            sub.plans.foreach(_.setChannel(chan))
            Channel.delete(oldChans.head.id)
          }
          Redirect(routes.Application.subscriberDashboard)
        }
      )
    )
  }

  def currentSubscriberId(implicit request: play.api.mvc.RequestHeader) = {
    val sessionSubscriberId = request.session.get("subscriber").getOrElse("0").toInt
    val identity = getCurrentIdentity
    val sub = Subscriber.findById(sessionSubscriberId)

    // anoymous users always return 0
    if (identity == "") {
      0
    // if the Subscriber is invalid return 0
    } else if (sub == None) {
      0
    // if the User is associated with the Subscriber, return stored value
    } else if (sub.get.userList().contains(User.findByIdentity(identity).get)) {
      sessionSubscriberId
    // if anything else happens, return 0
    } else {
      0
    }
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

  def userDashboard = isAuthenticated { identity => implicit request =>
    Ok(views.html.user.dashboard(identity))
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

  val planSetForm = Form(
    single(
      "channel_id" -> number
    )
  )

  def plan(id: Int) = isAuthenticated { identity => implicit request =>
    Plan.findById(id).map( plan =>
      subscriberMember(identity, plan.subscriber.get, Ok(views.html.plan.show(plan)))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber plan: " + id)))
  }

  def newPlan(sid: Int) = isAuthenticated { identity => implicit request =>
    subscriberMember(identity, Subscriber.findById(sid).get,
                   Ok(views.html.plan.create(sid, planForm)))
  }

  def createPlan(sid: Int) = isAuthenticated { identity => implicit request =>
    subscriberMember(identity, Subscriber.findById(sid).get, planForm.bindFromRequest.fold (
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
      if (plan.subscriber.get.userList().contains(identity)) {
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
      if (plan.subscriber.get.userList().contains(identity)) {
        Scheme.findById(schemeId).map( scheme => {
          plan.removeScheme(scheme)
          Redirect(routes.Application.editSubscriber(plan.subscriber.get.id))
        }).getOrElse(NotFound(views.html.static.trouble("No such scheme: " + schemeId)))
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }).getOrElse(NotFound(views.html.static.trouble("No such subscriber plan: " + id)))
  }

  def setPlanChannel(id: Int) = isAuthenticated { identity => implicit request =>
    Plan.findById(id).map( plan => {
      if (plan.subscriber.get.userList().contains(identity)) {
        planSetForm.bindFromRequest.fold (
          errors => BadRequest(views.html.subscriber.edit(plan.subscriber.get, errors)),
          value => {
            Channel.findById(value).map( chan => {
              plan.setChannel(chan)
              Redirect(routes.Application.editSubscriber(plan.subscriber.get.id))
            }).getOrElse(NotFound(views.html.static.trouble("No such channel: " + value)))
          }
        )
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }).getOrElse(NotFound(views.html.static.trouble("No such subscriber plan: " + id)))
  }

  def deletePlan(id: Int) = isAuthenticated { identity => implicit request =>
    Plan.findById(id).map( plan => {
      if (plan.subscriber.get.userList().contains(identity)) {
        Plan.delete(id)
        Redirect(routes.Application.subscriber(plan.subscriberId))
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber plan: " + id)))
  }

  def addSubscriberInterest(schemeTag: String) = isAuthenticated {
      identity => implicit request =>
    Subscriber.findById(currentSubscriberId).map( sub =>
      Scheme.findByTag(schemeTag).map( scheme => {
        val intValue = request.body.asFormUrlEncoded.get.get("interest").get.head
        val template = request.body.asFormUrlEncoded.get.get("template").get.head
        conveyor ! sub.addInterest(scheme, intValue, template.equals("true"))
        Redirect(routes.Application.interestBrowse("scheme", scheme.tag))
      }).getOrElse(NotFound(views.html.static.trouble("No such scheme tag: " + schemeTag)))
      ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + currentSubscriberId)))
  }

  def removeSubscriberInterest(iid: Int) = isAuthenticated {
      identity => implicit request =>
    Interest.findById(iid).map( interest => {
      if (interest.subscriber.userList().contains(identity)) {
        Interest.delete(interest.id)
        Redirect(routes.Application.interestBrowse("scheme", interest.schemeTag))
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }).getOrElse(NotFound(views.html.static.trouble("No such interest: " + iid)))
  }

  def interestBrowse(filter: String, value: String, page: Int) = isAuthenticated {
      identity => implicit request =>
    val subId = currentSubscriberId
    if (subId == 0) {
      NotFound(views.html.static.trouble("You must have a Subscriber defined for that operation."))
    } else {
      filter match {
        case "scheme" => Ok(views.html.interest.browse(subId, Interest.inScheme(subId, value, page), filter, value, page, Interest.schemeCount(subId, value)))
        case "plan" => Ok(views.html.interest.browse(subId, Interest.inPlan(subId, value.toInt, page), filter, value, page, Interest.planCount(subId, value.toInt)))
        case "match" => Ok(views.html.interest.browse(subId, Interest.inMatch(subId, value, page), filter, value, page, Interest.matchCount(subId, value)))
        case _ => NotFound(views.html.static.trouble("No such filter: " + filter))
      }
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

  def holdBrowse(id: Int, page: Int, exclude: Int) = isAuthenticated { identity => implicit request =>
    Subscriber.findById(id).map( sub =>
      subscriberMember(identity, sub,
                    Ok(views.html.hold.browse(sub.id, sub.holds(page, exclude), page, sub.holdCount(exclude))))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
  }

  def resolveHold(id: Int, accept: Boolean) = isAuthenticated { identity => implicit request =>
    val message = if(accept) {
                    "This item has been queued for delivery."
                  } else {
                    "Item has been removed from your delivery queue." }
    val message_type = if(accept) { "success" } else { "info" }
    Hold.findById(id).map( hold => {
      val sub = Subscriber.findById(hold.subscriberId).get
      if (sub.userList().contains(identity)) {
        conveyor ! (hold, accept)
        Redirect(routes.Application.holdBrowse(sub.id, 0, hold.itemId))
          .flashing(message_type -> message)
      } else {
        Unauthorized(views.html.static.trouble("You are not authorized"))
      }
    }
    ).getOrElse(NotFound(views.html.static.trouble("No such hold: " + id)))
  }

  def pickBrowse(id: Int, page: Int) = isAuthenticated { identity => implicit request =>
    Subscriber.findById(id).map( sub =>
      subscriberMember(identity, sub,
        Ok(views.html.topic_pick.browse(sub.id, sub.picks(page), page, sub.pickCount)))
    ).getOrElse(NotFound(views.html.static.trouble("No such subscriber: " + id)))
  }

  def resolvePick(id: Int, accept: Boolean) = isAuthenticated { identity => implicit request =>
    TopicPick.findById(id).map( pick => {
      val sub = Subscriber.findById(pick.subscriberId).get
      if (sub.userList().contains(identity)) {
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

  def reindexJob(dtype: String, key: String) = Action { implicit request =>
    val authorized_key = Play.configuration.getString("auth.harvest.key").get
    if (key == authorized_key) {
      indexer ! dtype
      Logger.info("Reindex Job for " + dtype + " started")
      Ok("Reindexing " + dtype + "s: started")
    } else {
      Logger.warn("A reindex tried to start without a valid key.")
      Unauthorized("Reindexing " + dtype + "s: not allowed")
    }
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
