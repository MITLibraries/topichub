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

/** Publisher is a provider of content to a hub, although it may not be the content originator:
  * it may be only represent a forwarding agent/proxy (e.g. another hub). Content is organized
  * into collections.
  *
  * @author richardrodgers
  */

case class Publisher(id: Int,  // DB key
                     userId: Int, // DB key of controlling user
                     tag: String, // Hub-unique tag
                     name: String, // Name of publisher
                     description: String,
                     category: String,  // descriptor: STM publisher, aggregator, repository, etc
                     status: String,
                     link: Option[String],  // Optional URL to publisher site
                     logo: Option[String],  // Optional URL to publisher logo
                     created: Date) {
  require(tag != null && name != null && description != null)

  def collectionCount = {
    DB.withConnection { implicit c =>
      SQL("select count(*) from collection where publisher_id = {id}").on('id -> id).as(scalar[Long].single)
    }
  }

  def itemCount = {
    DB.withConnection { implicit c =>
      if (collectionCount > 0L) {
        SQL("select sum(deposits) from collection where publisher_id = {id}").on('id -> id).as(scalar[Long].single)
      } else 0
    }
  }

  def harvests = {
    DB.withConnection { implicit c =>
      SQL("""
            SELECT *
            FROM harvest
            WHERE publisher_id = {id}
          """).on('id -> id).as(Harvest.harv *)
    }
  }

  def harvestCount = {
    DB.withConnection { implicit c =>
      SQL("select count(*) from harvest where publisher_id = {id}").on('id -> id).as(scalar[Long].single)
    }
  }
}

object Publisher {

  val pub = {
    get[Int]("id") ~ get[Int]("hub_user_id") ~ get[String]("tag") ~ get[String]("name") ~ get[String]("description") ~ get[String]("category") ~
    get[String]("status") ~ get[Option[String]]("link") ~ get[Option[String]]("logo") ~ get[Date]("created") map {
      case id ~ userId ~ tag ~ name ~ description ~ category ~ status ~ link ~ logo ~ created =>
        Publisher(id, userId, tag, name, description, category, status, link, logo, created)
    }
  }

  def findById(id: Int): Option[Publisher] = {
    DB.withConnection { implicit c =>
      SQL("select * from publisher where id = {id}").on('id -> id).as(pub.singleOpt)
    }
  }

  def findByTag(tag: String): Option[Publisher] = {
    DB.withConnection { implicit c =>
      SQL("select * from publisher where tag = {tag}").on('tag -> tag).as(pub.singleOpt)
    }
  }

  def all: List[Publisher] = {
    DB.withConnection { implicit c =>
      SQL("select * from publisher").as(pub *)
    }
  }

  def categories = {
    DB.withConnection { implicit c =>
      SQL("select distinct category from publisher").as(scalar[String] *)
    }
  }

  def categoryCount(category: String) = {
    DB.withConnection { implicit c =>
      SQL("select count(*) from publisher where category = {category}").on('category -> category).as(scalar[Long].single)
    }
  }

  def inCategory(category: String, page: Int) = {
    val offset = page * 10
    DB.withConnection { implicit c =>
      SQL(
        """
          select * from publisher where category = {category}
          order by created desc
          limit 10 offset {offset}
        """
      ).on('category -> category, 'offset -> offset).as(pub *)
    }
  }

  def create(userId: Int, tag: String, name: String, description: String, category: String, status: String, link: Option[String], logo: Option[String]) = {
    DB.withConnection { implicit c =>
      SQL("insert into publisher (hub_user_id, tag, name, description, category, status, link, logo, created) values ({hub_user_id}, {tag}, {name}, {description}, {category}, {status}, {link}, {logo}, {created})")
      .on('hub_user_id -> userId, 'tag -> tag, 'name -> name, 'description -> description, 'category -> category, 'status -> status, 'link -> link, 'logo -> logo, 'created -> new Date).executeInsert()
    }
  }

  def make(userId: Int, tag: String, name: String, description: String, category: String, status: String, link: Option[String], logo: Option[String]) = {
    findById(create(userId, tag, name, description, category, status, link, logo).get.toInt).get
  }

}
