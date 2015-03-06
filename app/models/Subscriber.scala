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

/** Subscriber is a consumer of content on a hub, where consumption may
  * range from email notifications, SWORD delivery of packages, etc
  *
  * @author richardrodgers
  */

case class Subscriber(id: Int,  // DB key
                      userId: Int, // DB key of controlling user
                      name: String, // Name of subscriber
                      category: String,  // descriptor: IR, hub, etc
                      contact: String,
                      link: Option[String],  // Optional URL to subscriber site
                      logo: Option[String],  // Optional URL to subscriber logo
                      created: Date) {

  def interests: List[Interest] = {
    DB.withConnection { implicit c =>
      SQL("select * from interest where subscriber_id = {id}").on('id -> id).as(Interest.interest *)
    }
  }

  def interestIn(schemeId: Int) = {
    DB.withConnection { implicit c =>
      SQL("select * from interest where subscriber_id = {sub_id} and scheme_id = {scheme_id}")
      .on('sub_id -> id, 'scheme_id -> schemeId).as(Interest.interest.singleOpt)
    }
  }

  def hasInterest(schemeId: Int) = interestIn(schemeId).isDefined

  def subscriptionFor(topicId: Int): Option[Subscription] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscription where subscriber_id = {sub_id} and topic_id = {topic_id} and active = true")
      .on('sub_id -> id, 'topic_id -> topicId).as(Subscription.subscrip.singleOpt)
    }
  }

  def subscribesTo(topicId: Int) = subscriptionFor(topicId).isDefined

  def subscribeTo(topic: Topic) = {
    Subscription.create(id, topic.id, interestIn(topic.scheme_id).get.action, new Date, new Date)
  }

  def interestsWithAction(action: String): List[Interest] = {
    DB.withConnection { implicit c =>
      SQL("select * from interest where action = {action} and subscriber_id = {id}")
      .on('action -> action, 'id -> id).as(Interest.interest *)
    }
  }

  // map of interests that could be added (not current interests)
  def newInterestMapView: Map[String, String] = {
    Scheme.all filter(_.gentype.equals("topic")) filter (sc => ! hasInterest(sc.id)) map (sc => sc.id.toString -> sc.tag) toMap
  }

  def addInterest(scheme: Scheme, action: String) = {
    Interest.create(id, scheme.id, action)
  }

  def removeInterest(scheme: Scheme) = {
    DB.withConnection { implicit c =>
      SQL("delete from interest where subscriber_id = {subscriber_id} and scheme_id = {scheme_id}")
      .on('subscriber_id -> id, 'scheme_id -> scheme.id).executeUpdate()
    }
  }

  def holdCount = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from hold where subscriber_id = {id}").on('id -> id).apply.head
      count[Long]("c")
    }
  }

  def holdOn(itemId: Int): Option[Hold] = {
    DB.withConnection { implicit c =>
      SQL("select * from hold where item_id = {item_id} and subscriber_id = {id}")
      .on('item_id -> itemId, 'id -> id).as(Hold.hold.singleOpt)
    }
  }

  def holds(page: Int) = {
    val offset = page * 10
    DB.withConnection { implicit c =>
      SQL(
        """
          select * from hold where subscriber_id = {sub_id}
          order by created desc
          limit 10 offset {offset}
        """
      ).on('sub_id -> id, 'offset -> offset).as(Hold.hold *)
    }
  }
}

object Subscriber {

  val sub = {
    get[Int]("id") ~ get[Int]("hub_user_id") ~ get[String]("name") ~ get[String]("category") ~
    get[String]("contact") ~ get[Option[String]]("link") ~ get[Option[String]]("logo") ~ get[Date]("created") map {
      case id ~ userId ~ name ~ category ~ contact ~ link ~ logo ~ created =>
        Subscriber(id, userId, name, category, contact, link, logo, created)
    }
  }

  def findById(id: Int): Option[Subscriber] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber where id = {id}").on('id -> id).as(sub.singleOpt)
    }
  }

  def findByUserId(uid: Int): Option[Subscriber] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber where hub_user_id = {uid}").on('uid -> uid).as(sub.singleOpt)
    }
  }

  def all: List[Subscriber] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber").as(sub *)
    }
  }

  def categories = {
    DB.withConnection { implicit c =>
      SQL("select distinct category from subscriber").as(scalar[String] *)
    }
  }

  def categoryCount(category: String) = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from subscriber where category = {category}").on('category -> category).apply.head
      count[Long]("c")
    }
  }

  def inCategory(category: String, page: Int) = {
    val offset = page * 10
    DB.withConnection { implicit c =>
      SQL(
        """
          select * from subscriber where category = {category}
          order by created desc
          limit 10 offset {offset}
        """
      ).on('category -> category, 'offset -> offset).as(sub *)
    }
  }

  def create(userId: Int, name: String, category: String, contact: String, link: Option[String], logo: Option[String]) = {
    DB.withConnection { implicit c =>
      SQL("insert into subscriber (hub_user_id, name, category, contact, link, logo, created) values ({hub_user_id}, {name}, {category}, {contact}, {link}, {logo}, {created})")
      .on('hub_user_id -> userId, 'name -> name, 'category -> category, 'contact -> contact, 'link -> link, 'logo -> logo, 'created -> new Date).executeInsert()
    }
  }

  def make(userId: Int, name: String, category: String, contact: String, link: Option[String], logo: Option[String]) = {
    findById(create(userId, name, category, contact, link, logo).get.toInt).get
  }

}
