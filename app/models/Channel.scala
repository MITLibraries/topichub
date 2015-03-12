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

/** Channel contains coordinates and credentials to effect a content transfer
  * or notification between a hub and a specified address or endpoint of a subscriber
  * or publisher.
  *
  * @author richardrodgers
  */

case class Channel(id: Int, // DB key
                   subscriberId: Int, // owning subscriber
                   protocol: String, // channel application/coounication protocol, e.g. SWORD-1.3
                   mode: String,     // delivery or notification
                   description: String, // description or label
                   userId: String,      // protocol service user
                   password: String,    // protocol service password
                   channelUrl: String,  // service endpoint
                   created: Date,       // creation of channel
                   updated: Date,       // last channel transfer
                   transfers: Int) {    // number of channel transfers

  def recordTransfer = {
    val newTrans = transfers + 1
    DB.withConnection { implicit c =>
      SQL("update channel set transfers = {transfers}, updated = {updated} where id = {id} ")
      .on('transfers -> newTrans, 'updated -> new Date, 'id -> id).executeUpdate()
    }
  }
}

object Channel {
  val channel = {
    get[Int]("id") ~ get[Int]("subscriber_id") ~ get[String]("protocol") ~ get[String]("mode") ~
    get[String]("description") ~ get[String]("user_id") ~ get[String]("password") ~
    get[String]("channel_url") ~ get[Date]("created") ~ get[Date]("updated") ~ get[Int]("transfers") map {
      case id ~  subscriberId ~ protocol ~ mode ~ description ~ userId ~ password ~ channelUrl ~ created ~ updated ~ transfers =>
        Channel(id, subscriberId, protocol, mode, description, userId, password, channelUrl, created, updated, transfers)
    }
  }

  def findById(id: Int): Option[Channel] = {
    DB.withConnection { implicit c =>
      SQL("select * from channel where id = {id}").on('id -> id).as(channel.singleOpt)
    }
  }

  def create(subscriberId: Int, protocol: String, mode: String, description: String, userId: String, password: String, channelUrl: String) = {
    DB.withConnection { implicit c =>
      SQL("insert into channel (subscriber_id, protocol, mode, description, user_id, password, channel_url, created, updated, transfers) values ({subscriberId}, {protocol}, {mode}, {description}, {userId}, {password}, {channelUrl}, {created}, {updated}, {transfers})")
      .on('subscriberId -> subscriberId, 'protocol -> protocol, 'mode -> mode, 'description -> description, 'userId -> userId, 'password -> password, 'channelUrl -> channelUrl, 'created -> new Date, 'updated -> new Date, 'transfers -> 0).executeInsert()
    }
  }

  def make(subscriberId: Int, protocol: String, mode: String, description: String, userId: String, password: String, channelUrl: String) = {
    findById(create(subscriberId, protocol, mode, description, userId, password, channelUrl).get.toInt).get
  }

  def delete(id: Int) {
    DB.withConnection { implicit c =>
      SQL("delete from channel where id = {id}").on('id -> id).executeUpdate()
    }
  }
}
