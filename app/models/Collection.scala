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

/** Collection is a content aggregation in which all elements (items)
  * share a content type and resource map. Each collections belongs to
  * a single Publisher.
  *
  * @author richardrodgers
  */

case class Collection(id: Int,
                      publisherId: Int,
                      ctypeId: Int,
                      resmapId: Int,
                      tag: String,
                      description: String,
                      policy: String,
                      created: Date,
                      updated: Date,
                      deposits: Int) {

  def recordDeposit {
    val newDep = deposits + 1
    DB.withConnection { implicit c =>
      SQL("update collection set deposits = {deposits} where id = {id} ")
          .on('deposits -> newDep, 'id -> id).executeUpdate()
    }
  }
}

object Collection {

  val coll = {
    get[Int]("id") ~ get[Int]("publisher_id") ~ get[Int]("content_type_id") ~ get[Int]("resource_map_id") ~
    get[String]("tag") ~ get[String]("description") ~ get[String]("policy") ~
    get[Date]("created") ~ get[Date]("updated") ~ get[Int]("deposits") map {
      case id ~ publisherId ~ ctypeId ~ resmapId ~ tag ~ description ~ policy ~ created  ~ updated ~ deposits =>
        Collection(id, publisherId, ctypeId, resmapId, tag, description, policy, created, updated, deposits)
    }
  }

  def findAll: List[Collection] = {
    DB.withConnection { implicit c =>
      SQL("select * from collection").as(coll *)
    }
  }

  def findById(id: Int): Option[Collection] = {
    DB.withConnection { implicit c =>
      SQL("select * from collection where id = {id}").on('id -> id).as(coll.singleOpt)
    }
  }

  def findByTag(tag: String): Option[Collection] = {
    DB.withConnection { implicit c =>
      SQL("select * from collection where tag = {tag}").on('tag -> tag).as(coll.singleOpt)
    }
  }

  def findByPublisher(pubId: Int): List[Collection] = {
    DB.withConnection { implicit c =>
      SQL("select * from collection where publisher_id = {pubId}").on('pubId -> pubId).as(coll *)
    }
  }

  def create(publisherId: Int, ctypeId: Int, resmapId: Int, tag: String, description: String, policy: String) = {
    val created = new Date
    val updated = created
		DB.withConnection { implicit c =>
			SQL("insert into collection (publisher_id, content_type_id, resource_map_id, tag, description, policy, created, updated, deposits) values ({publisher_id}, {ctype_id}, {resmap_id}, {tag}, {description}, {policy}, {created}, {updated}, {deposits})")
      .on('publisher_id -> publisherId, 'ctype_id -> ctypeId, 'resmap_id -> resmapId, 'tag -> tag, 'description -> description, 'policy -> policy, 'created -> created, 'updated -> updated, 'deposits -> 0).executeInsert()
		}
  }

  def make(publisherId: Int, ctypeId: Int, resmapId: Int, tag: String, description: String, policy: String): Collection = {
    findById(create(publisherId, ctypeId, resmapId, tag, description, policy).get.toInt).get
  }
}
