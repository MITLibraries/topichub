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

/** Hold is a subscription fulfillment in which an item is tagged for review
  * Following the review, the hold is released into a transfer, or discarded
  *
  * @author richardrodgers
  */

case class Hold(id: Int,  // DB key
                subscriberId: Int,   // DB key of subscriber
                subscriptionId: Int, // DB key of owning subscription
                itemId: Int,   // DB key of item held
                created: Date,  // when subscription established
                released: Date) {    // is subscription currently active

  def item = {
    DB.withConnection { implicit c =>
      SQL("select * from item where id = {item_id}").on('item_id -> itemId).as(Item.item.singleOpt).get
    }
  }

  def resolve(accept: Boolean) = {
    // not currently remembering state, so just delete
    DB.withConnection { implicit c =>
      SQL("delete from hold where id = {id}").on('id -> id).executeUpdate()
    }
  }
}

object Hold {

  def hold = {
    get[Int]("id") ~ get[Int]("subscriber_id") ~ get[Int]("subscription_id") ~ get[Int]("item_id") ~
    get[Date]("created") ~ get[Date]("released") map {
      case id ~ subscriberId ~ subscriptionId ~ itemId ~ created ~ released =>
        Hold(id, subscriberId, subscriptionId, itemId, created, released)
    }
  }

  def held(itemId: Int, subscriberId: Int) = {
    DB.withConnection { implicit c =>
      SQL("select * from hold where item_id = {item_id} and subscriber_id = {subscriber_id}")
      .on('item_id -> itemId, 'subscriber_id -> subscriberId).as(hold.singleOpt).isDefined
    }
  }

  def findById(id: Int): Option[Hold] = {
     DB.withConnection { implicit c =>
       SQL("select * from hold where id = {id}").on('id -> id).as(hold.singleOpt)
     }
   }

  def create(subscriberId: Int, subscriptionId: Int, itemId: Int) = {
    DB.withConnection { implicit c =>
      SQL("insert into hold (subscriber_id, subscription_id, item_id, created, released) values ({subscriber_id}, {subscription_id}, {item_id}, {created}, {released})")
      .on('subscriber_id -> subscriberId, 'subscription_id -> subscriptionId, 'item_id -> itemId, 'created -> new Date, 'released -> new Date).executeInsert()
    }
  }

  def make(subscriberId: Int, subscriptionId: Int, itemId: Int) = {
    findById(create(subscriberId, subscriptionId, itemId).get.toInt).get
  }
}
