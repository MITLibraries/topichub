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

/** Harvest describes a content source exposed via an Http 'pull' protocol
  * Harvested content typically is associated with a single publisher.
  *
  * @author richardrodgers
  */

case class Harvest(id: Int,             // DB key
                   publisherId: Int,    // Owning publisher
                   name: String,        // name
                   protocol: String,    // Protocol used to access source
                   serviceUrl: String,  // service URL for source
                   resourceUrl: String, // resource URL for source
                   freq: Int,           // harvest frequency in days
                   start: Date,         // earliest date to harvest
                   updated: Date) {     // last successful harvest date

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
      SQL("update harvest set updated = {updated} where id = {id}")
      .on('updated -> newDate, 'id -> id).executeUpdate()
    }
  }

  def harvestUrl = {
    // OAI-PMH date filters are inclusive on both ends (from and until),
    // so same from and until = 1 day. Thus a harvest starts from 1 day
    // past the last updated date thru freqency - 1 additional days (clear as mud?)
    val from = HubUtils.advanceDate(updated, 1)
    val until = HubUtils.advanceDate(from, freq - 1)
    serviceUrl + "?verb=ListIdentifiers&metadataPrefix=oai_dc" +
                 "&from=" + HubUtils.fmtDate(from) +
                 "&until=" + HubUtils.fmtDate(until)
  }
}

object Harvest {

  val harv = {
    get[Int]("id") ~ get[Int]("publisher_id") ~ get[String]("name") ~ get[String]("protocol") ~
    get[String]("service_url") ~ get[String]("resource_url") ~ get[Int]("freq") ~ get[Date]("start") ~ get[Date]("updated") map {
      case id ~ publisherId ~ name ~ protocol ~ serviceUrl ~ resourceUrl ~ freq ~ start ~ updated =>
        Harvest(id, publisherId, name, protocol, serviceUrl, resourceUrl, freq, start, updated)
    }
  }

  def findById(id: Int): Option[Harvest] = {
    DB.withConnection { implicit c =>
      SQL("select * from harvest where id = {id}").on('id -> id).as(harv.singleOpt)
    }
  }

  def findByPublisher(pubId: Int): List[Harvest] = {
    DB.withConnection { implicit c =>
      SQL("select * from harvest where publisher_id = {pubId}").on('pubId -> pubId).as(harv *)
    }
  }

  def create(publisherId: Int, name: String, protocol: String, serviceUrl: String, resourceUrl: String, freq: Int, start: Date) = {
    DB.withConnection { implicit c =>
      SQL("insert into harvest (publisher_id, name, protocol, service_url, resource_url, freq, start, updated) values ({publisher_id}, {name}, {protocol}, {service_url}, {resource_url}, {freq}, {start}, {updated})")
      .on('publisher_id -> publisherId, 'name -> name, 'protocol -> protocol, 'service_url -> serviceUrl, 'resource_url -> resourceUrl, 'freq -> freq, 'start -> start, 'updated -> start).executeInsert()
    }
  }

  def make(publisherId: Int, name: String, protocol: String, serviceUrl: String, resourceUrl: String, freq: Int, start: Date): Harvest = {
    findById(create(publisherId, name, protocol, serviceUrl, resourceUrl, freq, start).get.toInt).get
  }

  def delete(id: Int) {
    DB.withConnection { implicit c =>
      SQL("delete from harvest where id = {id}").on('id -> id).executeUpdate()
    }
  }
}
