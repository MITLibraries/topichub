/**
  * Copyright (c) 2015 MIT Libraries
  * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
  */
package models

import java.util.Date
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime
import java.sql.Timestamp
import java.time.YearMonth

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

  def interestIn(schemeTag: String) = {
    DB.withConnection { implicit c =>
      SQL("select * from interest where subscriber_id = {sub_id} and scheme_tag = {stag}")
      .on('sub_id -> id, 'stag -> schemeTag).as(Interest.interest.singleOpt)
    }
  }

  def hasInterest(schemeTag: String) = interestIn(schemeTag).isDefined

  def plannedFor(schemeId: Int): Boolean = {
    DB.withConnection { implicit c =>
      SQL("select plan_scheme.scheme_id from plan, plan_scheme where plan_scheme.scheme_id = {scheme_id} and plan_scheme.plan_id = plan.id and plan.subscriber_id = {sub_id}")
      .on('scheme_id -> schemeId, 'sub_id -> id).as(scalar[Int].singleOpt).isDefined
    }
  }

  def planFor(schemeId: Int) = {
    DB.withConnection { implicit c =>
      SQL("select plan.* from plan, plan_scheme where plan_scheme.scheme_id = {scheme_id} and plan_scheme.plan_id = plan.id and plan.subscriber_id = {sub_id}")
      .on('scheme_id -> schemeId, 'sub_id -> id).as(Plan.plan.singleOpt)
    }
  }

  def plans: List[Plan] = {
    DB.withConnection { implicit c =>
      SQL("select * from plan where subscriber_id = {id}").on('id -> id).as(Plan.plan *)
    }
  }

  def plannedSchemes: List[Scheme] = {
    DB.withConnection { implicit c =>
      SQL("select scheme.* from scheme, plan, plan_scheme where plan_scheme.plan_id = plan.id and plan_scheme.scheme_id = scheme.id and plan.subscriber_id = {sub_id}")
      .on('sub_id -> id).as(Scheme.scheme *)
    }
  }

  def subscriptionFor(topicId: Int): Option[Subscription] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscription where subscriber_id = {sub_id} and topic_id = {topic_id} and active = true")
      .on('sub_id -> id, 'topic_id -> topicId).as(Subscription.subscrip.singleOpt)
    }
  }

  def subscribesTo(topicId: Int) = subscriptionFor(topicId).isDefined

  def subscribeTo(topic: Topic): Subscription = {
    Subscription.make(id, topic.id, planFor(topic.scheme_id).get.fulfill, created, new Date)
  }

  def newItemCountFor(topicId: Int) = {
    DB.withConnection { implicit c =>
      val count = SQL(
        """
        select count(*) as c from item_topic
        where topic_id = {topic_id}
        and item_created > {created}
        and not exists (
          select 1 from transfer
          where subscriber_id = {sub_id}
          and item_id = item_topic.item_id
        )
        """
      ).on('topic_id -> topicId, 'created -> created, 'sub_id -> id).apply.head
      count[Long]("c")
    }
  }

  def templatesInScheme(schemeTag: String): List[Interest] = {
    DB.withConnection { implicit c =>
      SQL("select * from interest where subscriber_id = {id} and scheme_tag = {stag} and template = 'true'")
      .on('id -> id, 'stag -> schemeTag).as(Interest.interest *)
    }
  }

  def interestWithValue(schemeTag: String, intVal: String) = {
    DB.withConnection { implicit c =>
      SQL("select * from interest where subscriber_id = {id} and scheme_tag = {stag} and int_value = {intval} and template = 'false'")
      .on('id -> id, 'stag -> schemeTag, 'intval -> intVal).as(Interest.interest.singleOpt)
    }
  }

  // map of interests that could be added (not current interests)
  def newInterestMapView: Map[String, String] = {
    Scheme.all filter(_.gentype.equals("topic")) filter (sc => ! hasInterest(sc.tag)) map (sc => sc.id.toString -> sc.tag) toMap
  }

  // map of schemes that do not belong to any action plans
  def newPlanMapView: Map[String, String] = {
    Scheme.all.filter(_.gentype.equals("topic")).filter(sc => ! plannedFor(sc.id)).map(sc => sc.id.toString -> sc.tag) toMap
  }

  def addInterest(scheme: Scheme, intValue: String, template: Boolean = false) = {
    Interest.make(id, scheme.tag, intValue, template)
  }

  def removeInterest(scheme: Scheme, intValue: String) = {
    DB.withConnection { implicit c =>
      SQL("delete from interest where subscriber_id = {subscriber_id} and scheme_tag = {scheme_tag} and int_value = {int_value}")
      .on('subscriber_id -> id, 'scheme_tag -> scheme.tag, 'int_value -> intValue).executeUpdate()
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

  def pickCount = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from topic_pick where subscriber_id = {id}").on('id -> id).apply.head
      count[Long]("c")
    }
  }

  def picked(topicId: Int): Option[TopicPick] = {
    DB.withConnection { implicit c =>
      SQL("select * from topic_pick where topic_id = {topic_id} and subscriber_id = {id}")
      .on('topic_id -> topicId, 'id -> id).as(TopicPick.pick.singleOpt)
    }
  }

  def picks(page: Int) = {
    val offset = page * 10
    DB.withConnection { implicit c =>
      SQL(
        """
          select * from topic_pick where subscriber_id = {sub_id}
          order by created desc
          limit 10 offset {offset}
        """
      ).on('sub_id -> id, 'offset -> offset).as(TopicPick.pick *)
    }
  }

  def channels: List[Channel] = {
    DB.withConnection { implicit c =>
      SQL("select * from channel where subscriber_id = {id}").on('id -> id).as(Channel.channel *)
    }
  }

  def channelMapView: Map[String, String] = {
      channels map (ch => ch.id.toString -> ch.description) toMap
  }

  def monthlyTransferSummary(start: LocalDateTime, end: LocalDateTime) = {
    val monthyearformat = DateTimeFormatter.ofPattern("yyyy-M")
    var sDate = start
    val eDate = end
    val map = scala.collection.mutable.HashMap.empty[String,List[Int]]
    val md = scala.collection.mutable.ListBuffer.empty[String]
    val counts = scala.collection.mutable.ListBuffer.empty[List[Int]]

    while(!sDate.isAfter(eDate)) {
      // edate used in this should be the end of the month in which sDate is the first of the month
      map += (sDate.format(monthyearformat) ->
        transferCountByAction(
        sDate,
        YearMonth.of(sDate.getYear, sDate.getMonth).atEndOfMonth.atTime(23, 59, 59)
        )
      )
      md += sDate.format(monthyearformat)
      counts += transferCountByAction(sDate, sDate.plusMonths(1))
      sDate = sDate.plusMonths(1)
    }

    md.zip(counts).toList
  }

  def transferCountByAction(start: LocalDateTime, end: LocalDateTime) = {
    val stimestamp = Timestamp.valueOf(start)
    val etimestamp = Timestamp.valueOf(end)

    DB.withConnection { implicit c =>
      val rows = SQL(
        """
          SELECT count(*) as total,
            COALESCE(sum(CASE WHEN action = 'deliver' then 1 else 0 end), 0) deliverCount,
            COALESCE(sum(CASE WHEN action = 'discard' then 1 else 0 end), 0) discardCount
          FROM transfer
          WHERE subscriber_id = {id}
          AND created BETWEEN {stimestamp} AND {etimestamp}
        """).on('id -> id, 'stimestamp -> stimestamp, 'etimestamp -> etimestamp)
    rows().map(row => List(row[Int]("total"), row[Int]("deliverCount"), row[Int]("discardCount"))).head
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
