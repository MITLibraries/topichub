/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package models

import java.util.Date

import play.api.db.DB
import play.api._
import play.api.Play.current

import anorm.SqlParser._
import anorm.~
import anorm.SQL
import anorm.Row

/** Transfer is a subscription fulfillment involving typically pushing a
  * digitial copy of an item to a subscriber-defined destination
  *
  * @author richardrodgers
  */

case class Transfer(id: Int,  // DB key
                    subscriberId: Int,   // DB key of subscriber
                    subscriptionId: Int, // DB key of subscription transferred under
                    itemId: Int,   // DB key of item transferred
                    action: String,  // Subscription action to take
                    created: Date) {  // when subscription established
}

object Transfer {

  def transfer = {
    get[Int]("id") ~ get[Int]("subscriber_id") ~ get[Int]("subscription_id") ~ get[Int]("item_id") ~
    get[String]("action") ~ get[Date]("created") map {
      case id ~ subscriberId ~ subscriptionId ~ itemId ~ action ~ created =>
        Transfer(id, subscriberId, subscriptionId, itemId, action, created)
    }
  }

  def transferred(itemId: Int, subscriberId: Int) = {
    DB.withConnection { implicit c =>
      SQL("select * from transfer where item_id = {item_id} and subscriber_id = {subscriber_id}")
      .on('item_id -> itemId, 'subscriber_id -> subscriberId).as(transfer.singleOpt).isDefined
    }
  }

  def findById(id: Int): Option[Transfer] = {
     DB.withConnection { implicit c =>
       SQL("select * from transfer where id = {id}").on('id -> id).as(transfer.singleOpt)
     }
   }

  def create(subscriberId: Int, subscriptionId: Int, itemId: Int, action: String) = {
    DB.withConnection { implicit c =>
      SQL("insert into transfer (subscriber_id, subscription_id, item_id, action, created) values ({subscriber_id}, {subscription_id}, {item_id}, {action}, {created})")
      .on('subscriber_id -> subscriberId, 'subscription_id -> subscriptionId, 'item_id -> itemId, 'action -> action, 'created -> new Date).executeInsert()
    }
  }

  def make(subscriberId: Int, subscriptionId: Int, itemId: Int, action: String) = {
    findById(create(subscriberId, subscriptionId, itemId, action).get.toInt).get
  }
}
