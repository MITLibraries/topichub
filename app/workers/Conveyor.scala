/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import java.util.Date

import akka.actor.Actor

import play.api._
import play.api.Play.current
import play.api.libs.ws._
import play.api.mvc._
import play.api.http.HeaderNames._

import models.{Channel, Hold, Item, Scheme, Subscriber, Subscription, Topic, Transfer}
import services.Emailer

/** Conveyor is the worker responsible for transmitting notifications
  * or content packages to subscriber destinations
  *
  * @author richardrodgers
  */

class ConveyorWorker extends Actor {
  def receive = {
    case item: Item => Conveyor.newItem(item)
    case sub: Subscription => Conveyor.fulfill(sub)
    case (item: Item, subscr: Subscriber) => Conveyor.transferItem(item, subscr)
    case (hold: Hold, accept: Boolean) => Conveyor.resolveHold(hold, accept)
    case _ => println("I'm lost")
  }
}

object Conveyor {

  def newItem(item: Item) {
    // check all item's topics for subscriptions
    // if present, fuilfill them for this item
    var allSubs: List[Subscription] = List()
    item.topics.foreach(allSubs ::: _.subscriptions)
    // group by subscriber, so we can respect interests
    val mapSubs = allSubs.groupBy(_.subscriberId)
    val actMap = mapSubs.map { case (subId, subs) => (subId, subs.groupBy(_.action)) }
    actMap.map { case (subId, subMap) => processSubs(item, subMap) }
  }

  def fulfill(sub: Subscription) = {
    // transfer all eligible items ('latest' checking NYI)
    sub.topic.get.itemsSince(sub.earliest).foreach(convey(_, sub))
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
    sub.subscriber.map { subscr =>
      doTransfer(item, subscr, trans)
    }
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
    // TODO: need to read response, and determine success, also
    // update transfer object to indicate we are OK
  }

  private def notify(item: Item, sub: Subscription) = {
    sub.subscriber.map { subscr =>
      val text = views.txt.email.item_notify(item)
      val topic = sub.topic.get
      Emailer.notify(subscr.contact,
        "Scoap3Hub Alert - new in " + topic.tag + ":" + topic.name, text.body)
    }
  }
}
