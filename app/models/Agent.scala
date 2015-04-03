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

/** An agent is a software service that can perform operations for
  * benefit of subscribers, such as suggesting topics to subscribe to,
  * automatically subscribing or unsubscribing, etc
  *
  * @author richardrodgers
  */

case class Agent(id: Int,
                 tag: String,
                 label: String,
                 description: String,
                 code: String,
                 params: String,
                 icon: Option[String])

object Agent {

  val agent = {
    get[Int]("id") ~ get[String]("tag") ~ get[String]("label") ~ get[String]("description") ~
    get[String]("code") ~ get[String]("params") ~ get[Option[String]]("icon") map {
      case id ~ tag ~ label ~ description ~ code ~ params ~ icon => Agent(id, tag, label, description, code, params, icon)
    }
  }

  def create(tag: String, label: String, description: String, code: String, params: String, icon: Option[String]) = {
		DB.withConnection { implicit c =>
			SQL("insert into agent (tag, label, description, code, params, icon) values ({tag}, {label}, {description}, {code}, {params}, {icon})")
      .on('tag -> tag, 'label -> label, 'description -> description, 'code -> code, 'params -> params, 'icon -> icon).executeInsert()
		}
  }

  def make(tag: String, label: String, description: String, code: String, params: String, icon: Option[String]) = {
    findById(create(tag, label, description, code, params, icon).get.toInt).get
  }

  def all: List[Agent] = {
    DB.withConnection { implicit c =>
      SQL("select * from agent").as(agent *)
    }
  }

  def findById(id: Int): Option[Agent] = {
    DB.withConnection { implicit c =>
      SQL("select * from agent where id = {id}").on('id -> id).as(agent.singleOpt)
    }
  }

  def findByTag(tag: String): Option[Agent] = {
    DB.withConnection { implicit c =>
      SQL("select * from agent where tag = {tag}").on('tag -> tag).as(agent.singleOpt)
    }
  }

  def mapView: Map[String, String] = {
    all map (cp => cp.id.toString -> cp.tag) toMap
  }
}
