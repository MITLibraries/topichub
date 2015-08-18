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
import anorm.{Row, SQL, ~}

/** Subscriber is a consumer of content on a hub, where consumption may
  * range from email notifications, SWORD delivery of packages, etc
  *
  * @author richardrodgers
  */

case class Subscriber(id: Int,  // DB key
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

  def interestCountIn(schemeTag: String) = {
    DB.withConnection { implicit c =>
      SQL("""select COUNT(*) from interest
             where subscriber_id = {sub_id}
             and scheme_tag = {stag}""")
      .on('sub_id -> id, 'stag -> schemeTag).as(scalar[Long].single)
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

  def unplannedSchemes: List[Scheme] = {
    DB.withConnection { implicit c =>
      SQL(
        """
        select * from scheme
        where gentype = 'topic'
        and tag != 'meta'
        and not exists (
          select 1 from plan, plan_scheme
          where plan_scheme.plan_id = plan.id
          and plan_scheme.scheme_id = scheme.id
          and plan.subscriber_id = {sub_id}
        )
        """
      ).on('sub_id -> id).as(Scheme.scheme *)
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
      SQL(
        """
        select count(*) from item_topic
        where topic_id = {topic_id}
        and item_created > {created}
        and not exists (
          select 1 from transfer
          where subscriber_id = {sub_id}
          and item_id = item_topic.item_id
        )
        """
      ).on('topic_id -> topicId, 'created -> created, 'sub_id -> id).as(scalar[Long].single)
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
    interestWithValue(scheme.tag, intValue).map(i => Interest.delete(i.id))
  }

  def holdCount(exclude: Int=0) = {
    DB.withConnection { implicit c =>
      SQL("""
            select count(*) from hold
            where subscriber_id = {id}
            and item_id != {exclude}
          """).on('id -> id, 'exclude -> exclude).as(scalar[Long].single)
    }
  }

  def holdOn(itemId: Int): Option[Hold] = {
    DB.withConnection { implicit c =>
      SQL("select * from hold where item_id = {item_id} and subscriber_id = {id}")
      .on('item_id -> itemId, 'id -> id).as(Hold.hold.singleOpt)
    }
  }

  def holds(page: Int, exclude: Int=0) = {
    val offset = page * 10
    DB.withConnection { implicit c =>
      SQL(
        """
          select * from hold where subscriber_id = {sub_id}
          and item_id != {exclude}
          order by created desc
          limit 10 offset {offset}
        """
      ).on('sub_id -> id, 'offset -> offset, 'exclude -> exclude).as(Hold.hold *)
    }
  }

  def pickCount = {
    DB.withConnection { implicit c =>
      SQL("select count(*) from topic_pick where subscriber_id = {id}").on('id -> id).as(scalar[Long].single)
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

    def transferCount(total: Int, deliverCount: Int, discardCount: Int) = {
      List(total, deliverCount, discardCount)
    }

    val transferCount_parser = {
        get[Int] ("total") ~
        get[Int] ("deliverCount") ~
        get[Int] ("discardCount") map({
          case total~deliverCount~discardCount => transferCount(total, deliverCount, discardCount)
        })
    }

    DB.withConnection { implicit c =>
      SQL(
        """
          SELECT count(*) as total,
            COALESCE(sum(CASE WHEN action = 'deliver' then 1 else 0 end), 0) deliverCount,
            COALESCE(sum(CASE WHEN action = 'discard' then 1 else 0 end), 0) discardCount
          FROM transfer
          WHERE subscriber_id = {id}
          AND created BETWEEN {stimestamp} AND {etimestamp}
        """).on('id -> id, 'stimestamp -> stimestamp, 'etimestamp -> etimestamp)
        .as(transferCount_parser single)
    }
  }

  def linkUser(userId: Int, approved: Boolean = false, admin: Boolean = false) = {
    DB.withConnection { implicit c =>
      SQL("""
          INSERT INTO hub_user_subscriber (hub_user_id, subscriber_id, admin, approved)
          VALUES ({userId}, {id}, {admin}, {approved})
          """)
      .on('userId -> userId, 'id -> id, 'admin -> admin, 'approved -> approved).executeUpdate()
    }
  }

  def unlinkUser(userId: Int) = {
    DB.withConnection { implicit c =>
      SQL("""
          DELETE FROM hub_user_subscriber
          WHERE hub_user_id = {userId}
          AND subscriber_id = {id}
          """)
      .on('userId -> userId, 'id -> id).executeUpdate()
    }
  }

  // Default is to only return approved users. Pass approved = false to get a list
  // of users that are pending administrative Approval.
  def userList(approved: Boolean = true): List[User] = {
    DB.withConnection { implicit c =>
      SQL(
        """
          SELECT hub_user.* FROM hub_user
          JOIN hub_user_subscriber ON hub_user.id = hub_user_subscriber.hub_user_id
          WHERE hub_user_subscriber.subscriber_id = {id}
          AND approved = {approved}
        """
      ).on('id -> id, 'approved -> approved).as(User.user *)
    }
  }

  def adminList: List[User] = {
    DB.withConnection { implicit c =>
      SQL(
        """
          SELECT hub_user.* FROM hub_user
          JOIN hub_user_subscriber ON hub_user.id = hub_user_subscriber.hub_user_id
          WHERE hub_user_subscriber.subscriber_id = {id}
          AND admin = true
        """
      ).on('id -> id).as(User.user *)
    }
  }

  def approveUser(userId: Int) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          UPDATE hub_user_subscriber
          SET approved = true
          WHERE hub_user_id = {userId}
          AND subscriber_id = {id}
        """
      ).on('id -> id, 'userId -> userId).executeUpdate
    }
  }

  def denyUser(userId: Int) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          DELETE FROM hub_user_subscriber
          WHERE hub_user_id = {userId}
          AND subscriber_id = {id}
        """
      ).on('id -> id, 'userId -> userId).executeUpdate
    }
  }

  def makeAdmin(userId: Int) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          UPDATE hub_user_subscriber
          SET admin = true
          WHERE hub_user_id = {userId}
          AND subscriber_id = {id}
        """
      ).on('id -> id, 'userId -> userId).executeUpdate
    }
  }

  def removeAdmin(userId: Int) = {
    DB.withConnection { implicit c =>
      SQL(
        """
          UPDATE hub_user_subscriber
          SET admin = false
          WHERE hub_user_id = {userId}
          AND subscriber_id = {id}
        """
      ).on('id -> id, 'userId -> userId).executeUpdate
    }
  }

  def delete = {
    DB.withConnection { implicit c =>
      SQL(
        """
          DELETE FROM hub_user_subscriber
          WHERE subscriber_id = {id};
        """
      ).on('id -> id).executeUpdate
    }
    DB.withConnection { implicit c =>
      SQL(
        """
          DELETE FROM subscriber
          WHERE id = {id};
        """
      ).on('id -> id).executeUpdate
    }
  }
}

object Subscriber {

  val sub = {
    get[Int]("id") ~ get[String]("name") ~ get[String]("category") ~
    get[String]("contact") ~ get[Option[String]]("link") ~ get[Option[String]]("logo") ~ get[Date]("created") map {
      case id ~ name ~ category ~ contact ~ link ~ logo ~ created =>
        Subscriber(id, name, category, contact, link, logo, created)
    }
  }

  def findById(id: Int): Option[Subscriber] = {
    DB.withConnection { implicit c =>
      SQL("select * from subscriber where id = {id}").on('id -> id).as(sub.singleOpt)
    }
  }

  def findByUserId(uid: Int): List[Subscriber] = {
    DB.withConnection { implicit c =>
      SQL("""
            SELECT subscriber.* FROM subscriber
            JOIN hub_user_subscriber ON hub_user_subscriber.subscriber_id = subscriber.id
            WHERE hub_user_id = {uid}
            AND hub_user_subscriber.approved = true"""
          ).on('uid -> uid).as(sub *)
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
      SQL("select count(*) from subscriber where category = {category}")
      .on('category -> category).as(scalar[Long].single)
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

  def create(name: String, category: String, contact: String, link: Option[String], logo: Option[String]) = {
    DB.withConnection { implicit c =>
      SQL("insert into subscriber (name, category, contact, link, logo, created) values ({name}, {category}, {contact}, {link}, {logo}, {created})")
      .on('name -> name, 'category -> category, 'contact -> contact, 'link -> link, 'logo -> logo, 'created -> new Date).executeInsert()
    }
  }

  def make(userId: Int, name: String, category: String, contact: String, link: Option[String], logo: Option[String]) = {
    val sub = findById(create(name, category, contact, link, logo).get.toInt).get
    sub.linkUser(userId, approved = true, admin = true)
    sub
  }

}
