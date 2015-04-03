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

/** TopicPick is a topic suggestion (potential subscription) for a subscriber
  * Following a review, the pick is resolved to a subscription, or discarded
  *
  * @author richardrodgers
  */

case class TopicPick(id: Int,  // DB key
                     subscriberId: Int,   // DB key of subscriber
                     topicId: Int,   // DB key of selected topic
                     agentId: Int, // DB key of agent that created it
                     created: Date,  // when pick created
                     resolved: Date) {    // when pick resolved

  def topic = {
    DB.withConnection { implicit c =>
      SQL("select * from topic where id = {topic_id}").on('topic_id -> topicId).as(Topic.topic.singleOpt).get
    }
  }

  def resolve(accept: Boolean) = {
    // not currently remembering state, so just delete
    DB.withConnection { implicit c =>
      SQL("delete from topic_pick where id = {id}").on('id -> id).executeUpdate()
    }
  }
}

object TopicPick {

  val pick = {
    get[Int]("id") ~ get[Int]("subscriber_id") ~ get[Int]("topic_id") ~ get[Int]("agent_id") ~
    get[Date]("created") ~ get[Date]("resolved") map {
      case id ~ subscriberId ~ topicId ~ agentId ~ created ~ resolved =>
        TopicPick(id, subscriberId, topicId, agentId, created, resolved)
    }
  }

  def picked(topicId: Int, subscriberId: Int) = {
    DB.withConnection { implicit c =>
      SQL("select * from topic_pick where topic_id = {topic_id} and subscriber_id = {subscriber_id}")
      .on('topic_id -> topicId, 'subscriber_id -> subscriberId).as(pick.singleOpt).isDefined
    }
  }

  def findById(id: Int): Option[TopicPick] = {
     DB.withConnection { implicit c =>
       SQL("select * from topic_pick where id = {id}").on('id -> id).as(pick.singleOpt)
     }
   }

  def create(subscriberId: Int, topicId: Int, agentId: Int) = {
    DB.withConnection { implicit c =>
      SQL("insert into topic_pick (subscriber_id, topic_id, agent_id, created, resolved) values ({subscriber_id}, {topic_id}, {agent_id}, {created}, {resolved})")
      .on('subscriber_id -> subscriberId, 'topic_id -> topicId, 'agent_id -> agentId, 'created -> new Date, 'resolved -> new Date).executeInsert()
    }
  }

  def make(subscriberId: Int, topicId: Int, agentId: Int) = {
    findById(create(subscriberId, topicId, agentId).get.toInt).get
  }
}
