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

/** ContentFormat represents a format of an artifact (file) type.
  *
  * @author richardrodgers
  */

case class ContentFormat(id: Int, tag: String, label: String, description: String, url: String, mimetype: String, logo: Option[String]) {

/*
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
  */
}

object ContentFormat {

  val format = {
    get[Int]("id") ~ get[String]("tag") ~ get[String]("label") ~ get[String]("description") ~
    get[String]("url") ~ get[String]("mimetype") ~ get[String]("logo") map {
      case id ~ tag ~ label ~ description ~ url ~ mimetype ~ logo => ContentFormat(id, tag, label, description, url, mimetype, Some(logo))
    }
  }

  def create(tag: String, label: String, description: String, url: String, mimetype: String, logo: Option[String]) = {
		DB.withConnection { implicit c =>
			SQL("insert into content_format (tag, label, description, url, mimetype, logo) values ({tag}, {label}, {description}, {url}, {mimetype}, {logo})")
      .on('tag -> tag, 'label -> label, 'description -> description, 'url -> url, 'mimetype -> mimetype, 'logo -> logo).executeInsert()
		}
  }

  def make(tag: String, label: String, description: String, url: String, mimetype: String, logo: Option[String]): ContentFormat = {
    findById(create(tag, label, description, url, mimetype, logo).get.toInt).get
  }

  def all: List[ContentFormat] = {
    DB.withConnection { implicit c =>
      SQL("select * from content_format").as(format *)
    }
  }

  def findById(id: Int): Option[ContentFormat] = {
    DB.withConnection { implicit c =>
      SQL("select * from content_format where id = {id}").on('id -> id).as(format.singleOpt)
    }
  }

  def findByTag(tag: String): Option[ContentFormat] = {
    DB.withConnection { implicit c =>
      SQL("select * from content_format where tag = {tag}").on('tag -> tag).as(format.singleOpt)
    }
  }

  def mapView: Map[String, String] = {
      all map (ct => ct.id.toString -> ct.tag) toMap
  }
}
