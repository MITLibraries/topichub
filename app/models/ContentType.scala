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

/** ContentType represents a content type.
  *
  * @author richardrodgers
  */

case class ContentType(id: Int, tag: String, label: String, description: String, logo: Option[String]) {

  def schemes(relation: String): List[Scheme] = {
    DB.withConnection { implicit c =>
      SQL("select scheme.* from scheme, content_type_scheme, content_type where scheme.id = content_type_scheme.scheme_id and content_type_scheme.content_type_id = content_type.id and content_type.id = {ctype_id} and content_type_scheme.relation = {relation}")
      .on('ctype_id -> id, 'relation -> relation).as(Scheme.scheme *)
    }
  }

  def addScheme(scheme: Scheme, relation: String) {
    DB.withConnection { implicit c =>
      SQL("insert into content_type_scheme (content_type_id, scheme_id, relation) values ({ctype_id}, {scheme_id}, {relation})")
      .on('ctype_id -> id, 'scheme_id -> scheme.id, 'relation -> relation).executeUpdate()
    }
  }

  def removeScheme(scheme: Scheme, relation: String) {
    DB.withConnection { implicit c =>
      SQL("delete from content_type_scheme where content_type_id = {ctype_id} and scheme_id = {scheme_id} and relation = {relation}")
      .on('ctype_id -> id, 'scheme_id -> scheme.id, 'relation -> relation).executeUpdate()
    }
  }
}

object ContentType {

  val ctype = {
    get[Int]("id") ~ get[String]("tag") ~ get[String]("label") ~ get[String]("description") ~ get[String]("logo") map {
      case id ~ tag ~ label ~ description ~ logo => ContentType(id, tag, label, description, Some(logo))
    }
  }

  def create(tag: String, label: String, description: String, logo: Option[String]) = {
		DB.withConnection { implicit c =>
			SQL("insert into content_type (tag, label, description, logo) values ({tag}, {label}, {description}, {logo})")
      .on('tag -> tag, 'label -> label, 'description -> description, 'logo  -> logo).executeInsert()
		}
  }

  def make(tag: String, label: String, description: String, logo: Option[String]): ContentType = {
    findById(create(tag, label, description, logo).get.toInt).get
  }

  def all: List[ContentType] = {
    DB.withConnection { implicit c =>
      SQL("select * from content_type").as(ctype *)
    }
  }

  def findById(id: Int): Option[ContentType] = {
    DB.withConnection { implicit c =>
      SQL("select * from content_type where id = {id}").on('id -> id).as(ctype.singleOpt)
    }
  }

  def findByTag(tag: String): Option[ContentType] = {
    DB.withConnection { implicit c =>
      SQL("select * from content_type where tag = {tag}").on('tag -> tag).as(ctype.singleOpt)
    }
  }

  def mapView: Map[String, String] = {
      all map (ct => ct.id.toString -> ct.tag) toMap
  }
}
