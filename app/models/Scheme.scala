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

/**
 * Scheme is the controlling vocabulary for a given set of topics or other values
 *
 * @author richardrodgers
 */

case class Scheme(id: Int, tag: String, gentype: String, category: String, description: String,
                  link: Option[String], logo: Option[String], created: Date) {

  def topicCount = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from topic where scheme_id = {id}").on('id -> id).apply.head
      count[Long]("c")
    }
  }

  def validator: Option[Validator] = {
    DB.withConnection { implicit c =>
      SQL("select * from validator where scheme_id = {id}").on('id -> id).as(Validator.validator.singleOpt)
    }
  }
}

object Scheme {

  val scheme = {
    get[Int]("id") ~ get[String]("tag") ~ get[String]("gentype") ~ get[String]("category") ~
    get[String]("description") ~ get[String]("link") ~ get[String]("logo") ~ get[Date]("created") map {
      case id ~ tag ~ gentype ~ category ~ description ~ link ~ logo ~ created =>
        Scheme(id, tag, gentype, category, description, Some(link), Some(logo), created)
    }
  }

  def create(tag: String, gentype: String, category: String, description: String, link: Option[String], logo: Option[String]) {
		DB.withConnection { implicit c =>
			SQL("insert into scheme (tag, gentype, category, description, link, logo, created) values ({tag}, {gentype}, {category}, {description}, {link}, {logo}, {created})")
      .on('tag -> tag, 'gentype -> gentype, 'category -> category, 'description -> description, 'link -> link, 'logo -> logo, 'created -> new Date).executeUpdate()
		}
  }

  def all: List[Scheme] = {
    DB.withConnection { implicit c =>
      SQL("select * from scheme").as(scheme *)
    }
  }

  def withGentype(gentype: String): List[Scheme] = {
    DB.withConnection { implicit c =>
      SQL("select * from scheme where gentype = {gentype}").on('gentype -> gentype).as(scheme *)
    }
  }

 def findById(id: Int): Option[Scheme] = {
    DB.withConnection { implicit c =>
      SQL("select * from scheme where id = {id}").on('id -> id).as(scheme.singleOpt)
    }
  }

  def findByTag(tag: String): Option[Scheme] = {
    DB.withConnection { implicit c =>
      SQL("select * from scheme where tag = {tag}").on('tag -> tag).as(scheme.singleOpt)
    }
  }

  def mapView: Map[String, String] = {
    all map (sc => sc.id.toString -> sc.tag) toMap
  }
}
