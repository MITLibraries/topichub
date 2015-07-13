/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package models

import java.util.Date
import java.time.{Instant, LocalDateTime, ZoneId}

import play.api.db.DB
import play.api._
import play.api.Play.current

import anorm.SqlParser._
import anorm.~
import anorm.SQL
import anorm.Row

/** Cull contains parameters for performing a time- and policy-based removal
  * of publisher content from the hub - items and entailed topics, essentially.
  * The only policy currently supported is a 'soft'/'weak' cull, which
  * only removes items for which there is no associated subscriber activity.
  *
  * @author richardrodgers
  */

case class Cull(id: Int,                   // DB key
                publisherId: Int,          // Owning publisher
                name: String,              // name
                policy: String,            // rules for cull item selection
                notifyUrl: Option[String], // optional URL for publisher notification
                freq: Int,                 // cull frequency in days
                start: Date,               // earliest date to cull
                updated: Date) {           // last successful cull date

  def publisher = {
    DB.withConnection { implicit c =>
      SQL("select * from publisher where id = {pub_id}")
      .on('pub_id -> publisherId).as(Publisher.pub.singleOpt)
    }
  }

  def complete = {
    // advance updated field with freq
    val newDate = HubUtils.advanceDate(updated, freq)
    DB.withConnection { implicit c =>
      SQL("update cull set updated = {updated} where id = {id}")
      .on('updated -> newDate, 'id -> id).executeUpdate()
    }
  }
}

object Cull {

  val cull = {
    get[Int]("id") ~ get[Int]("publisher_id") ~ get[String]("name") ~ get[String]("policy") ~
    get[Option[String]]("notify_url") ~ get[Int]("freq") ~ get[Date]("start") ~ get[Date]("updated") map {
      case id ~ publisherId ~ name ~ policy ~ notifyUrl ~ freq ~ start ~ updated =>
        Cull(id, publisherId, name, policy, notifyUrl, freq, start, updated)
    }
  }

  def all: List[Cull] = {
    DB.withConnection { implicit c =>
      SQL("SELECT * FROM cull").as(cull *)
    }
  }

  def findById(id: Int): Option[Cull] = {
    DB.withConnection { implicit c =>
      SQL("select * from cull where id = {id}").on('id -> id).as(cull.singleOpt)
    }
  }

  def findByPublisher(pubId: Int): List[Cull] = {
    DB.withConnection { implicit c =>
      SQL("select * from cull where publisher_id = {pubId}").on('pubId -> pubId).as(cull *)
    }
  }

  def create(publisherId: Int, name: String, policy: String, notifyUrl: Option[String], freq: Int, start: Date) = {
    DB.withConnection { implicit c =>
      SQL("insert into cull (publisher_id, name, policy, notify_url, freq, start, updated) values ({publisher_id}, {name}, {policy}, {notify_url}, {freq}, {start}, {updated})")
      .on('publisher_id -> publisherId, 'name -> name, 'policy -> policy, 'notify_url -> notifyUrl, 'freq -> freq, 'start -> start, 'updated -> start).executeInsert()
    }
  }

  def make(publisherId: Int, name: String, policy: String, notifyUrl: Option[String], freq: Int, start: Date): Cull = {
    findById(create(publisherId, name, policy, notifyUrl, freq, start).get.toInt).get
  }

  def delete(id: Int) {
    DB.withConnection { implicit c =>
      SQL("delete from cull where id = {id}").on('id -> id).executeUpdate()
    }
  }
}
