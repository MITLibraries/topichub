/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package models

import java.util.Date

import play.api.db.DB
import play.api.Play.current

import anorm.SqlParser._
import anorm.~
import anorm.SQL

case class Finder(id: Int, schemeId: Int, formatId: Int, description: String, cardinality: String,
                  idKey: String, idLabel: String, author: String, created: Date) {

  def format: Option[ContentFormat] = {
    DB.withConnection { implicit c =>
      SQL("select * from content_format where id = {id}").on('id -> formatId).as(ContentFormat.format.singleOpt)
    }
  }

}

object Finder {

  val finder = {
      get[Int]("id") ~ get[Int]("scheme_id") ~ get[Int]("content_format_id") ~ get[String]("description") ~
      get[String]("cardinality") ~ get[String]("id_key") ~ get[String]("id_label") ~ get[String]("author") ~ get[Date]("created") map {
      case id ~ schemeId ~ formatId ~ description ~ cardinality ~ idKey ~ idLabel ~ author ~ created =>
        Finder(id, schemeId, formatId, description, cardinality, idKey, idLabel, author, created)
    }
  }

  def create(schemeId: Int, formatId: Int, description: String, cardinality: String,
             idKey: String, idLabel: String, author: String) = {
		DB.withConnection { implicit c =>
			SQL(
        """
        insert into finder (scheme_id, content_format_id, description, cardinality, id_key, id_label, author, created)
        values ({scheme_id}, {format_id}, {description}, {cardinality}, {idKey}, {idLabel}, {author}, {created})
        """
      ).on('scheme_id -> schemeId, 'format_id -> formatId, 'description -> description, 'cardinality -> cardinality,
           'idKey -> idKey, 'idLabel -> idLabel, 'author -> author, 'created -> new Date)
      .executeInsert()
		}
  }

  def make(schemeId: Int, formatId: Int, description: String, cardinality: String,
             idKey: String, idLabel: String, author: String): Finder = {
    findById(create(schemeId, formatId, description, cardinality, idKey, idLabel, author).get.toInt).get
  }

  def findById(id: Int): Option[Finder] = {
    DB.withConnection { implicit c =>
      SQL("select * from finder where id = {id}").on('id -> id).as(finder.singleOpt)
    }
  }

  def findByScheme(schemeId: Int): List[Finder] = {
    DB.withConnection { implicit c =>
      SQL("select * from finder where scheme_id = {scheme_id}").on('scheme_id -> schemeId).as(finder *)
    }
  }

  def forSchemeAndFormat(schemeId: Int, formatId: Int): List[Finder] = {
    DB.withConnection { implicit c =>
      SQL("select * from finder where scheme_id = {scheme_id} and content_format_id = {format_id}")
      .on('scheme_id -> schemeId,'format_id -> formatId).as(finder *)
    }
  }

  def delete(id: Int) {
  	DB.withConnection { implicit c =>
  		SQL("delete from finder where id = {id}").on('id -> id).executeUpdate()
  	}
  }
}
