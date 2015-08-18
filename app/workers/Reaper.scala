/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package workers

import java.util.Date

import akka.actor.{Actor, Props}
import play.api._
import models.{Collection, Cull, HubUtils, Item, Topic}
import services.Emailer

/** Reaper is a worker responsible for performing content culls
  * and other removal operations.
  *
  * @author richardrodgers
  */

class ReaperWorker extends Actor {
  def receive = {
    case c: Cull => Reaper.cull(c)
    case (oid: String, policy: String) => Reaper.expungeItem(oid, policy)
    case _ => Logger.error("Unhandled Case in ReaperWorker#receive")
  }
}

object Reaper {

  def cull(cull: Cull) = {
    // search for publisher-supplied items within cull date range
    val until = HubUtils.advanceDate(cull.updated, cull.freq)
    val items = Item.inPublisherRange(cull.publisherId, cull.updated, until)
    // expunge all items allowed by the policy
    items.filter(i => allow(i, cull.policy)).foreach(expunge(_, cull.notifyUrl))
  }

  def expungeItem(oid: String, policy: String) = {
    Item.findByKey(oid).map { item =>
      if (allow(item, policy)) expunge(item, None)
    }
  }

  def allow(item: Item, policy: String): Boolean = {
    policy match {
      case "soft" => itemQuiet(item) && topicsQuiet(item)
      case _ => Logger.error("Unknown policy"); false
    }
  }

  def itemQuiet(item: Item): Boolean = {
    // quiet means no transfers or holds in hub for item
    item.transfers == 0 && item.holds == 0L
  }

  def topicsQuiet(item: Item): Boolean = {
    // quiet means no item topic has subscriptions or picks on it in hub
    item.topics.forall(t => t.subscriptionCount == 0L && t.pickCount == 0L)
  }

  def expunge(item: Item, notifyUrl: Option[String]) = {
    // first remove from item index
    Indexer.deindex(item)
    // now examine item's topics - deindex and remove any that
    // will become 'orphaned' (no other items have them)
    item.topics.filter(_.itemCount == 1).map { topic =>
      Indexer.deindex(topic)
      Topic.delete(topic.id)
    }
    // If a notificaton URL has been registered, use it
    // NB: only 'mailto:' URLs supported currently
    if (notifyUrl.isDefined && notifyUrl.get.startsWith("mailto:")) {
      val recip = notifyUrl.get.substring(7)
      val msg = views.txt.email.item_removed(item)
      Emailer.notify(recip, s"Item Removed from ${HubUtils.siteName}", msg.body)
    }
    // Finally delete item itself
    Item.delete(item.id)
  }
}
