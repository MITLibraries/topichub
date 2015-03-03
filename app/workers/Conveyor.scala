/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import akka.actor.Actor

import play.api._
import play.api.Play.current
import play.api.libs.ws._

import models.{Hold, Item, Subscriber, Subscription, Topic, Transfer}
import services.Emailer

/** Conveyor is the worker responsible for transmitting notifications
  * or content packages to subscriber destinations
  *
  * @author richardrodgers
  */

class ConveyorWorker extends Actor {
  def receive = {
    case item: Item => Conveyor.newItem(item)
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
    //val channel = Channel.findById(sub.channel_id).get
    //transferItem(transfer, item, channel, Some(topic))
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
      // create a new transfer or hold
      sub.action match {
        case "deliver" => Transfer.make(sub.subscriberId, sub.id, item.id, sub.action)
        case "review" => Hold.make(sub.subscriberId, sub.id, item.id)
        case "notify" => println("email")
        case _ => println("Unknown action: " + sub.action)
      }
      // Fake all conveyance actions with an email notification (for now)
      val subscriber = Subscriber.findById(sub.subscriberId).get
      val topic = Topic.findById(sub.topicId).get
      val text = views.txt.email.item_notify(item)
      Emailer.notify(subscriber.contact, "Scoap3Hub Alert - new in: " + topic.name, text.body)
    }
  }
}
