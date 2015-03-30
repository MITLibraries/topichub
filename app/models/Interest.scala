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

/** Interest represents a subscriber intent toward values (topics) in a namespace (scheme).
  * The intent specifies a type of action to be taken when items for the topics appear:
  * 'notify' (email), 'review' (add to a pick list), or 'deliver' (SWORD deposit)
  *
  * @author richardrodgers
  */

case class Interest(id: Int, subscriberId: Int, schemeId: Int, action: String,
                    created: Date) {

  def subscriber = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber where id = {subscriber_id}")
      .on('subscriber_id -> subscriberId).as(Subscriber.sub.singleOpt)
    }
  }

  def scheme = {
    DB.withConnection { implicit c =>
      SQL("select * from scheme where id = {scheme_id}")
      .on('scheme_id -> schemeId).as(Scheme.scheme.singleOpt)
    }
  }
}

object Interest {

  val interest = {
    get[Int]("id") ~ get[Int]("subscriber_id") ~ get[Int]("scheme_id") ~
    get[String]("action") ~ get[Date]("created") map {
      case id ~ subscriberId ~ schemeId ~ action ~ created =>
        Interest(id, subscriberId, schemeId, action, created)
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
