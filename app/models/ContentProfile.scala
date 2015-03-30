/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package models

import play.api.db.DB
import play.api._
import play.api.Play.current

import anorm.SqlParser._
import anorm.~
import anorm.SQL

/** ContentProfile consists of a set of content (Item) descriptors,
  * identified by their scheme
  *
  * @author richardrodgers
  */

case class ContentProfile(id: Int, tag: String, label: String, description: String) {

  def schemes: List[Scheme] = {
    DB.withConnection { implicit c =>
      SQL("select scheme.* from scheme, content_profile_scheme where scheme.id = content_profile_scheme.scheme_id and content_profile_scheme.content_profile_id = {cprof_id}")
      .on('cprof_id -> id).as(Scheme.scheme *)
    }
  }

  def addScheme(scheme: Scheme) = {
    if (! schemes.contains(scheme)) {
      DB.withConnection { implicit c =>
        SQL("insert into content_profile_scheme (content_profile_id, scheme_id) values ({cprof_id}, {scheme_id})")
        .on('cprof_id -> id, 'scheme_id -> scheme.id).executeInsert()
      }
    }
  }

  def removeScheme(scheme: Scheme) = {
    DB.withConnection { implicit c =>
      SQL("delete from content_profile_scheme where content_profile_id = {cprof_id} and scheme_id = {scheme_id}")
      .on('cprof_id -> id, 'scheme_id -> scheme.id).executeUpdate()
    }
  }
}

object ContentProfile {

  val cprof = {
    get[Int]("id") ~ get[String]("tag") ~ get[String]("label") ~ get[String]("description") map {
      case id ~ tag ~ label ~ description => ContentProfile(id, tag, label, description)
    }
  }

  def create(tag: String, label: String, description: String) = {
		DB.withConnection { implicit c =>
			SQL("insert into content_profile (tag, label, description) values ({tag}, {label}, {description})")
      .on('tag -> tag, 'label -> label, 'description -> description).executeUpdate()
		}
  }

  def all: List[ContentProfile] = {
    DB.withConnection { implicit c =>
      SQL("select * from content_profile").as(cprof *)
    }
  }

  def findById(id: Int): Option[ContentProfile] = {
    DB.withConnection { implicit c =>
      SQL("select * from content_profile where id = {id}").on('id -> id).as(cprof.singleOpt)
    }
  }

  def findByTag(tag: String): Option[ContentProfile] = {
    DB.withConnection { implicit c =>
      SQL("select * from content_profile where tag = {tag}").on('tag -> tag).as(cprof.singleOpt)
    }
  }

  def mapView: Map[String, String] = {
    all map (cp => cp.id.toString -> cp.tag) toMap
  }
}
