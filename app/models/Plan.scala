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

/** Plan represents a set of subscriber intentions toward values related to a
  * scheme (namespace). The intention specifies a type of action to be taken for
  * various model types, e.g. when items for a topic in an enrolled scheme appear
  * (the 'fulfill' action): 'notify' (email), 'review' (add to a hold list),
  * or 'deliver' (SWORD deposit)
  *
  * @author richardrodgers
  */

case class Plan(id: Int,                // DB key
                subscriberId: Int,      // owning subscriber
                channelId: Int,         // channel used for delivery actions
                name: String,           // name of plan
                description: String,    // indication of plan concerns
                icon: String,           // UI indication of plan
                fulfill: String,        // action for subscription fulfillment (deliver, review, notify)
                pick: String,           // action for topic selection (subscribe, review, notify)
                interest: String,       // action for when interests match topics (subscribe, review)
                template: String,       // action for when interest templates match topics
                created: Date) {        // when plan was created

  def subscriber = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber where id = {subscriber_id}")
      .on('subscriber_id -> subscriberId).as(Subscriber.sub.singleOpt)
    }
  }

  def channel = {
    DB.withConnection { implicit c =>
      SQL("select * from channel where id = {channel_id}")
      .on('channel_id -> channelId).as(Channel.channel.singleOpt)
    }
  }

  def addScheme(scheme: Scheme) = {
    DB.withConnection { implicit c =>
      SQL("insert into plan_scheme (plan_id, scheme_id, created) values ({plan_id}, {scheme_id}, {created})")
      .on('plan_id -> id, 'scheme_id -> scheme.id, 'created -> new Date).executeUpdate()
    }
  }

  def removeScheme(scheme: Scheme) = {
    DB.withConnection { implicit c =>
      SQL("delete from plan_scheme where plan_id = {plan_id} and scheme_id = {scheme_id}")
      .on('plan_id -> id, 'scheme_id -> scheme.id).executeUpdate()
    }
  }

  def schemes: List[Scheme] = {
    DB.withConnection { implicit c =>
      SQL("select scheme.* from scheme, plan_scheme where scheme.id = plan_scheme.scheme_id and plan_scheme.plan_id = {plan_id}")
      .on('plan_id -> id).as(Scheme.scheme *)
    }
  }

  def setChannel(chan: Channel) = {
    DB.withConnection { implicit c =>
      SQL("update plan set channel_id = {chan_id} where id = {plan_id}")
      .on('chan_id -> chan.id, 'plan_id -> id).executeUpdate()
    }
  }
}

object Plan {

  val plan = {
    get[Int]("id") ~ get[Int]("subscriber_id") ~ get[Int]("channel_id") ~
    get[String]("name") ~ get[String]("description") ~ get[String]("icon") ~
    get[String]("fulfill") ~ get[String]("pick") ~ get[String]("interest") ~
    get[String]("template") ~ get[Date]("created") map {
      case id ~ subscriberId ~ channelId ~ name ~ description ~ icon ~ fulfill ~ pick ~ interest ~ template ~ created =>
        Plan(id, subscriberId, channelId, name, description, icon, fulfill, pick, interest, template, created)
    }
  }

  def findById(id: Int): Option[Plan] = {
    DB.withConnection { implicit c =>
      SQL("select * from plan where id = {id}").on('id -> id).as(plan.singleOpt)
    }
  }

  def findBySubscriber(sid: Int): List[Plan] = {
    DB.withConnection { implicit c =>
      SQL("select * from plan where subscriber_id = {sid}").on('sid -> sid).as(plan *)
    }
  }

  def create(subscriberId: Int, channelId: Int, name: String, description: String,
             icon: String, fulfill: String, pick: String, interest: String, template: String) = {
    val created = new Date
    DB.withConnection { implicit c =>
      SQL(
        """
        insert into plan (subscriber_id, channel_id, name, description, icon, fulfill, pick, interest, template, created)
        values ({subscriber_id}, {channel_id}, {name}, {description}, {icon}, {fulfill}, {pick}, {interest}, {template}, {created})
        """
      ).on('subscriber_id -> subscriberId, 'channel_id -> channelId, 'name -> name, 'description -> description,
           'icon -> icon, 'fulfill -> fulfill, 'pick -> pick, 'interest -> interest, 'template -> template, 'created -> created).executeInsert()
    }
  }

  def make(subscriberId: Int, channelId: Int, name: String, description: String,
           icon: String, fulfill: String, pick: String, interest: String, template: String) = {
    findById(create(subscriberId, channelId, name, description, icon, fulfill, pick, interest, template).get.toInt).get
  }

  def delete(id: Int) {
    DB.withConnection { implicit c =>
      SQL("delete from plan where id = {id}").on('id -> id).executeUpdate()
    }
  }
}
