/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import java.util.Date

import akka.actor.Actor
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import play.api._
import play.api.Play.current
import play.api.libs.ws._
import play.api.mvc._
import play.api.http.HeaderNames._

import models.{Agent, Channel, Hold, Interest, Item, Plan, Scheme, Subscriber,
               Subscription, Topic, TopicPick, Transfer, User}
import services.Emailer

/** Conveyor is the worker responsible for transmitting notifications
  * or content packages to subscriber destinations
  *
  * @author richardrodgers
  */

class ConveyorWorker extends Actor {
  def receive = {
    case item: Item => Conveyor.newItem(item)
    case topic: Topic => Conveyor.newTopic(topic)
    case interest: Interest => Conveyor.newInterest(interest)
    case sub: Subscription => Conveyor.newSubscription(sub)
    case (item: Item, subscr: Subscriber) => Conveyor.transferItem(item, subscr)
    case (hold: Hold, accept: Boolean) => Conveyor.resolveHold(hold, accept)
    case (pick: TopicPick, accept: Boolean) => Conveyor.resolvePick(pick, accept)
    case _ => println("I'm lost")
  }
}

object Conveyor {

  def newItem(item: Item) = {
    // check all item's topics for subscriptions
    // if present, fuilfill them for this item
    var allSubs: List[Subscription] = List()
    item.topics.foreach(allSubs ::: _.subscriptions)
    // group by subscriber, so we can respect interests
    val mapSubs = allSubs.groupBy(_.subscriberId)
    val actMap = mapSubs.map { case (subId, subs) => (subId, subs.groupBy(_.action)) }
    actMap.map { case (subId, subMap) => processSubs(item, subMap) }
  }

  def newTopic(topic: Topic) = {
    // check for unmatched interests in this topic, grouped by Subscriber for efficiency
    val interests = Interest.unmatched(topic.scheme.tag).groupBy(_.subscriberId)
    interests.map { case (subId, ints) => processInterests(subId, topic, ints) }
    // the same for interest templates
    val templates = Interest.templates(topic.scheme.tag).groupBy(_.subscriberId)
    templates.map { case (subId, tints) => processTemplates(subId, topic, tints) }
  }

  private def processInterests(subscriberId: Int, topic: Topic, ints: List[Interest]) = {
    val sub = Subscriber.findById(subscriberId).get
    val mints = ints.filter(_.intValue == topic.tag)
    sub.planFor(topic.scheme.id).map { plan =>
      plan.interest match {
        case "subscribe" => subscribe(plan.fulfill)
        case "review" => review()
        case _ => println("Don't go there")
      }
    }

    def subscribe(action: String) = mints.foreach( i =>
      Subscription.create(i.subscriberId, topic.id, action, sub.created, new Date))

    def review() = mints.foreach( i =>
      TopicPick.create(i.subscriberId, topic.id, conveyorAgent.id))
  }

  private def processTemplates(subscriberId: Int, topic: Topic, tints: List[Interest]) = {
    val sub = Subscriber.findById(subscriberId).get
    val mints = tints.filter(tmp => topic.tag.contains(tmp.intValue))
    sub.planFor(topic.scheme.id).map { plan =>
      plan.template match {
        case "subscribe" => subscribe(plan.fulfill)
        case "review" => review()
        case _ => println("Don't go there")
      }
    }

    def subscribe(action: String) = mints.foreach( i =>
      Subscription.create(i.subscriberId, topic.id, action, sub.created, new Date))

    def review() = {
      mints.foreach( t =>
        TopicPick.create(t.subscriberId, topic.id, conveyorAgent.id))
    }
  }

  def newSubscription(sub: Subscription) = {
    // create and/or link backing interest
    val subscr = sub.subscriber
    val topic = sub.topic
    val scheme = topic.scheme
    val interest = subscr.interestWithValue(scheme.tag, topic.tag).
                   getOrElse(subscr.templatesInScheme(scheme.tag).find(t => topic.tag.contains(t.intValue)).
                   getOrElse(subscr.addInterest(scheme, topic.tag, false)))
    sub.linkInterest(interest)
    // transfer all eligible items ('latest' checking NYI)
    topic.itemsSince(sub.earliest).foreach(convey(_, sub))
  }

  def newInterest(interest: Interest) = {
    // find the plan governing this interest (ignore if not in a plan)
    val sub = interest.subscriber
    val scheme = interest.scheme
    val plan = sub.planFor(scheme.id)
    if (plan.isDefined) {
      plan.get.interest match {
        case "review" => reviewInterest(interest, plan.get)
        case _ => println("Don't go there")
      }
    }
  }

  private def reviewInterest(interest: Interest, plan: Plan) = {
    if (interest.template) {
      // look through all topics attempting a match
      val scheme = interest.scheme
      var idx = 0
      var mPage = Topic.withScheme(scheme.id, idx)
      while (! mPage.isEmpty) {
        mPage.filter(_.tag.contains(interest.intValue)).foreach { topic =>
          TopicPick.create(interest.subscriberId, topic.id, conveyorAgent.id)
        }
        idx += 1
        mPage = Topic.withScheme(scheme.id, idx)
      }
    } else {
      // try to find this value as an exact match
      val topic = Topic.forSchemeAndTag(interest.schemeTag, interest.intValue)
      if (topic.isDefined) {
        // add to topic picks for review
        TopicPick.create(interest.subscriberId, topic.get.id, conveyorAgent.id)
      }
    }
  }

  private def conveyorAgent = {
    Agent.findByTag("conveyor").getOrElse(
      Agent.make("conveyor", "Conveyor Agent", "Internal", "", "", None)
    )
  }

  def transferItem(item: Item, subscr: Subscriber) = {
    // one-off (not subscription-based) transfer request from a subscriber
    // presumed to be a delivery, but this could be generalized if needed
    val subTopic = Topic.forSchemeAndTag("meta", "picks:" + subscr.id).getOrElse(
      Topic.make(Scheme.findByTag("meta").get.id, "picks:" + subscr.id, subscr.name))
    // user effectively manually assigns this item to the topic by requesting it
    if (! item.hasTopic(subTopic)) item.addTopic(subTopic)
    val sub = Subscription.withSubscriberAndTopic(subscr.id, subTopic.id).headOption.getOrElse(
      Subscription.make(subscr.id, subTopic.id, "deliver", subscr.created, new Date))
    convey(item, sub)
  }

  def resolveHold(hold: Hold, accept: Boolean) = {
    // keep a record as a transfer even if discarding
    val action = if (accept) "deliver" else "discard"
    val trans = Transfer.make(hold.subscriberId, hold.subscriptionId, hold.itemId, action)
    if (accept) {
      transfer(hold.item, trans)
    }
    // clean up hold in any case
    hold.resolve(accept)
  }

  def resolvePick(pick: TopicPick, accept: Boolean) = {
    // keep a record as a cancelled subscription? TODO
    if (accept) {
      val sub = Subscriber.findById(pick.subscriberId).get
      val plan = sub.planFor(pick.topic.scheme_id)
      newSubscription(Subscription.make(sub.id, pick.topicId, plan.get.pick, sub.created, new Date))
    }
    // clean up pick in any case
    pick.resolve(accept)
  }

  private def processSubs(item: Item, subMap: Map[String, List[Subscription]]) = {
    // process in confidence order - deliver, review, notify
    subMap.get("deliver").getOrElse(List()).foreach(convey(item,_))
    subMap.get("review").getOrElse(List()).foreach(convey(item,_))
    subMap.get("notify").getOrElse(List()).foreach(convey(item,_))
  }

  private def convey(item: Item, sub: Subscription) = {
    // skip if already transfered/held
    if (! Transfer.transferred(item.id, sub.subscriberId) &&
        ! Hold.held(item.id, sub.subscriberId)) {
      // create a new transfer or hold, or notify subscriber
      sub.action match {
        case "deliver" => transfer(item, sub)
        case "review" => Hold.make(sub.subscriberId, sub.id, item.id)
        case "notify" => notify(item, sub)
        case _ => println("Unknown action: " + sub.action)
      }
    }
  }

  private def transfer(item: Item, sub: Subscription) = {
    val trans = Transfer.make(sub.subscriberId, sub.id, item.id, sub.action)
    doTransfer(item, sub.subscriber, trans)
  }

  private def transfer(item: Item, trans: Transfer) = {
    Subscriber.findById(trans.subscriberId).map { subscr =>
      doTransfer(item, subscr, trans)
    }
  }

  private def doTransfer(item: Item, subscr: Subscriber, trans: Transfer) = {
    // should fail if no channel - TODO
    subscr.channels.headOption.map { chan =>
      chan.protocol match {
        case "sword" => swordTransfer(item, chan, trans)
        case _ => println("Don't know how to transfer via: " + chan.protocol)
      }
    }
  }

  private def swordTransfer(item: Item, channel: Channel, trans: Transfer) = {
    var req = WS.url(channel.channelUrl)
      .withHeaders(CONTENT_TYPE -> "application/zip",
                  "X-packaging" -> "http://purl.org/net/sword-types/METSDSpaceSIP")
      .withAuth(channel.userId, channel.password, WSAuthScheme.BASIC)
    println("About to deposit: " + req)
    val resp = req.post(Packager.packageItemAsFile(item))

    resp onComplete {
      case Success(response) => readSwordResponse(response)
      case Failure(t) => failedSwordTransferAttempt(t.getMessage)
    }

    def failedSwordTransferAttempt(t: String) = {
      println("An error occurred attempting to submit a Sword package")
      val sysadminEmails = User.allByRole("sysadmin").map(x => x.email).mkString(",")
      val msg = views.txt.email.sword_transfer_failure(item, channel, trans, t).body
      sendSwordFailureEmail(sysadminEmails, msg)
    }

    def sendSwordFailureEmail(addresses: String, msg: String) = {
      println(msg)
      Emailer.notify(addresses, "SCOAP3Hub: failure of sword delivery detected", msg)
      Transfer.delete(trans.id)
    }

    def readSwordResponse(response: play.api.libs.ws.WSResponse) = {
      if (response.status == 201) {
        println("Successful Transfer of " + item.objKey)
        channel.recordTransfer
      } else {
        println("The SWORD server did not accept the transfer. Response was " + response.toString)
        // email admin details
        val sysadminEmails = User.allByRole("sysadmin").map(x => x.email).mkString(",")
        val msg = views.txt.email.sword_transfer_failure(item, channel, trans, response.toString).body
        sendSwordFailureEmail(sysadminEmails, msg)
      }
    }
  }

  private def notify(item: Item, sub: Subscription) = {
    val text = views.txt.email.item_notify(item)
    val topic = sub.topic
    Emailer.notify(sub.subscriber.contact,
        "Scoap3Hub Alert - new in " + topic.tag + ":" + topic.name, text.body)
  }
}
