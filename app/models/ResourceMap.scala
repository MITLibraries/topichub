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
import anorm.Row

/** ResourceMap provides an information map of a package containing item content.
  * It locates scheme resources via package-relative file names.
  *
  * @author richardrodgers
  */

case class ResourceMap(id: Int, tag: String, description: String, swordUrl: Option[String]) {

  def addMapping(schemeId: Int, formatId: Int, source: String, rank: Int) {
    DB.withConnection { implicit c =>
      SQL("insert into resource_map_scheme (resource_map_id, scheme_id, content_format_id, source, rank) values ({resmap_id}, {scheme_id}, {format_id}, {source}, {rank})")
      .on('resmap_id -> id, 'scheme_id -> schemeId, 'format_id -> formatId, 'source -> source, 'rank -> rank).executeUpdate()
    }
  }

  def mappingsForScheme(scheme: Scheme): List[(String, Int, Int)] = {
    DB.withConnection { implicit c =>
      SQL("select source,content_format_id,rank from resource_map_scheme where scheme_id = {scheme_id} and resource_map_id = {resmap_id}")
      .on('scheme_id -> scheme.id, 'resmap_id -> id).as(str("source") ~ int("content_format_id") ~ int("rank") map(flatten) *)
    }
  }

  def schemes: List[Scheme] = {
    DB.withConnection { implicit c =>
      SQL("select scheme.* from scheme, resource_map_scheme, resource_map where scheme.id = resource_map_scheme.scheme_id and resource_map_scheme.resource_map_id = resource_map.id and resource_map.id = {resmap_id}")
      .on('resmap_id -> id).as(Scheme.scheme *)
    }
  }

  def removeMapping(scheme: Scheme, source: String) {
    DB.withConnection { implicit c =>
      SQL("delete from resource_map_scheme where resource_map_id = {resmap_id} and scheme_id = {scheme_id} and source = {source}")
      .on('resmap_id -> id, 'scheme_id -> scheme.id, 'source -> source).executeUpdate()
    }
  }
}

object ResourceMap {

  val resmap = {
    get[Int]("id") ~ get[String]("tag") ~ get[String]("description") ~ get[String]("sword_url") map {
      case id ~ tag ~ description ~ swordUrl => ResourceMap(id, tag, description, Some(swordUrl))
    }
  }

  def create(tag: String, description: String, swordUrl: Option[String]) {
		DB.withConnection { implicit c =>
			SQL("insert into resource_map (tag, description, sword_url) values ({tag}, {description}, {swordUrl})")
      .on('tag -> tag, 'description -> description, 'swordUrl -> swordUrl).executeUpdate()
		}
  }

  def all: List[ResourceMap] = {
    DB.withConnection { implicit c =>
      SQL("select * from resource_map").as(resmap *)
    }
  }

  def findById(id: Int): Option[ResourceMap] = {
    DB.withConnection { implicit c =>
      SQL("select * from resource_map where id = {id}").on('id -> id).as(resmap.singleOpt)
    }
  }

  def findByTag(tag: String): Option[ResourceMap] = {
    DB.withConnection { implicit c =>
      SQL("select * from resource_map where tag = {tag}").on('tag -> tag).as(resmap.singleOpt)
    }
  }

  def mapView: Map[String, String] = {
      all map (rm => rm.id.toString -> rm.tag) toMap
  }
}
