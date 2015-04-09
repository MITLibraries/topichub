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

/** Interest represents a subscriber intent toward a value in a namespace (scheme).
  * The value may be exact, or represent a 'template' or pattern that
  * describes a value space.
  *
  * @author richardrodgers
  */

case class Interest(id: Int, subscriberId: Int, schemeTag: String, intValue: String,
                    template: Boolean, matched: Int, created: Date) {

  def subscriber = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber where id = {subscriber_id}")
      .on('subscriber_id -> subscriberId).as(Subscriber.sub.singleOpt)
    }
  }

  def scheme = {
    DB.withConnection { implicit c =>
      SQL("select * from scheme where tag = {scheme_tag}")
      .on('scheme_tag -> schemeTag).as(Scheme.scheme.singleOpt)
    }
  }
}

object Interest {

  val interest = {
    get[Int]("id") ~ get[Int]("subscriber_id") ~ get[String]("scheme_tag") ~ get[String]("int_value") ~
    get[Boolean]("template") ~ get[Int]("matched") ~ get[Date]("created") map {
      case id ~ subscriberId ~ schemeTag ~ intValue ~ template ~ matched ~ created =>
        Interest(id, subscriberId, schemeTag, intValue, template, matched, created)
    }
  }

  def findById(id: Int): Option[Interest] = {
    DB.withConnection { implicit c =>
      SQL("select * from interest where id = {id}").on('id -> id).as(interest.singleOpt)
    }
  }

  def findBySubscriber(sid: Int): List[Interest] = {
    DB.withConnection { implicit c =>
      SQL("select * from interest where subscriber_id = {sid}").on('sid -> sid).as(interest *)
    }
  }

  def withScheme(schemeId: Int, page: Int): List[Interest] = {
      val offset = page * 10
      DB.withConnection { implicit c =>
      SQL(
        """
          select * from interest
          where scheme_id = {scheme_id}
          order by id
          limit 10 offset {offset}
        """
      ).on('scheme_id -> schemeId, 'offset -> offset).as(interest *)
    }
  }

  def create(subscriberId: Int, schemeId: Int, action: String) = {
    val created = new Date
    DB.withConnection { implicit c =>
      SQL(
        """
        insert into interest (subscriber_id, scheme_id, action, created)
        values ({subscriber_id}, {scheme_id}, {action}, {created})
        """
      ).on('subscriber_id -> subscriberId, 'scheme_id -> schemeId, 'action -> action, 'created -> created).executeInsert()
    }
  }

  def make(subscriberId: Int, schemeId: Int, action: String): Interest = {
    findById(create(subscriberId, schemeId, action).get.toInt).get
  }
}
