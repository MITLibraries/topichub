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

/** Topic represents a value in a namespace (its scheme). Topics are assigned
  * to items, and become the object of subscriptions.
  *
  * @author richardrodgers
  */

case class Topic(id: Int, scheme_id: Int, tag: String, name: String,
                 link: Option[String], created: Date, updated: Date, transfers: Int) {

  def recentItems(max: Int) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select item.* from item, item_topic
          where item.id = item_topic.item_id and item_topic.topic_id = {topic_id}
          order by item.created desc limit {max}
        """
      ).on('topic_id -> id, 'max -> max).as(Item.item *)
    }
  }

  def itemsSince(start: Date) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select item.* from item, item_topic
          where item.id = item_topic.item_id and
          item_topic.topic_id = {topic_id} and
          item.created > {created}
          order by item.created asc
        """
      ).on('topic_id -> id, 'created -> start).as(Item.item *)
    }
  }

  def pagedItems(page: Int, max: Int) = {
    val offset = page * max
    DB.withConnection { implicit c =>
      SQL(
        """
          select item.* from item, item_topic
          where item.id = item_topic.item_id and item_topic.topic_id = {topic_id}
          order by item.created desc
          limit {max} offset {offset}
        """
      ).on('topic_id -> id, 'max -> max, 'offset -> offset).as(Item.item *)
    }
  }

  def itemCount = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from item_topic where topic_id = {id}").on('id -> id).apply.head
      count[Long]("c")
    }
  }

  def scheme = {
    DB.withConnection { implicit c =>
      SQL("select * from scheme where id = {scheme_id}")
      .on('scheme_id -> scheme_id).as(Scheme.scheme.singleOpt)
    }
  }

  def subscriptionCount = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from subscription where topic_id = {id}").on('id -> id).apply.head
      count[Long]("c")
    }
  }

  def subscriptions = {
    DB.withConnection { implicit c =>
      SQL("select * from subscription where topic_id = {id}").on('id -> id).as(Subscription.subscrip *)
    }
  }
}

object Topic {

  val topic = {
    get[Int]("id") ~ get[Int]("scheme_id") ~ get[String]("tag") ~ get[String]("name") ~
    get[Option[String]]("link") ~ get[Date]("created") ~ get[Date]("updated") ~ get[Int]("transfers") map {
      case id ~ scheme_id ~ tag ~ name ~ link ~ created ~ updated ~ transfers =>
        Topic(id, scheme_id, tag, name, link, created, updated, transfers)
    }
  }

  def findById(id: Int): Option[Topic] = {
    DB.withConnection { implicit c =>
      SQL("select * from topic where id = {id}").on('id -> id).as(topic.singleOpt)
    }
  }

  def all: List[Topic] = {
    DB.withConnection { implicit c =>
      SQL("select * from topic").as(topic *)
    }
  }

  def withScheme(scheme_id: Int, page: Int): List[Topic] = {
      val offset = page * 10
      DB.withConnection { implicit c =>
      SQL(
        """
          select * from topic
          where scheme_id = {scheme_id}
          order by tag
          limit 10 offset {offset}
        """
      ).on('scheme_id -> scheme_id, 'offset -> offset).as(topic *)
    }
  }

  def create(schemeId: Int, tag: String, name: String) = {
    val created = new Date
    val updated = created
    DB.withConnection { implicit c =>
      SQL(
        """
        insert into topic (scheme_id, tag, name, created, updated, transfers)
        values ({scheme_id}, {tag}, {name}, {created}, {updated}, {transfers})
        """
      ).on('scheme_id -> schemeId, 'tag -> tag, 'name -> name, 'created -> created, 'updated -> updated, 'transfers -> 0).executeInsert()
    }
  }

  def make(schemeId: Int, tag: String, name: String): Topic = {
    findById(create(schemeId, tag, name).get.toInt).get
  }

  def forSchemeAndTag(schemeTag: String, topicTag: String): Option[Topic] = {
    DB.withConnection { implicit c =>
      SQL("select topic.* from scheme, topic where scheme.id = topic.scheme_id and scheme.tag = {schemeTag} and topic.tag = {topicTag}")
      .on('schemeTag -> schemeTag, 'topicTag -> topicTag).as(topic.singleOpt)
    }
  }

  def deleteUnlinkedBefore(date: Date) = {
    DB.withConnection { implicit c =>
      SQL("delete from topic where created < {created}").on('created -> date)
    }
  }
}
