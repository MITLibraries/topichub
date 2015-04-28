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

case class Interest(id: Int, subscriberId: Int, schemeTag: String,
                    intValue: String, template: Boolean, created: Date) {

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
    get[Int]("id") ~ get[Int]("subscriber_id") ~ get[String]("scheme_tag") ~
    get[String]("int_value") ~ get[Boolean]("template") ~ get[Date]("created") map {
      case id ~ subscriberId ~ schemeTag ~ intValue ~ template ~ created =>
        Interest(id, subscriberId, schemeTag, intValue, template, created)
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

  // todo: maybe a better name would be schemeInterestCount? The current name makes me expect a count
  // of schemes... which would always be 1 as we pass in the Scheme to count.
  def schemeCount(subscriberId: Int, schemeTag: String) = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from interest where interest.subscriber_id = {sub_id} and interest.scheme_tag = {sch_tag}")
      .on('sub_id -> subscriberId, 'sch_tag -> schemeTag).apply.head
      count[Long]("c")
    }
  }

  def inScheme(subscriberId: Int, schemeTag: String, page: Int): List[Interest] = {
      val offset = page * 10
      DB.withConnection { implicit c =>
      SQL(
        """
          select * from interest
          where subscriber_id = {sub_id} and scheme_tag = {scheme_tag}
          order by id
          limit 10 offset {offset}
        """
      ).on('sub_id -> subscriberId, 'scheme_tag -> schemeTag, 'offset -> offset).as(interest *)
    }
  }

  // todo: maybe a better name would be planInterestCount? The current name makes me expect a count
  // of schemes... which would always be 1 as we pass in the Scheme to count.
  def planCount(subscriberId: Int, planId: Int) = {
    DB.withConnection { implicit c =>
      val count = SQL(
        """
        select count(interest.*) as c from interest, scheme, plan_scheme
        where interest.subscriber_id = {sub_id}
        and interest.scheme_tag = scheme.tag
        and plan_scheme.scheme_id = scheme.id
        and plan_scheme.plan_id = {plan_id}
        """
      ).on('sub_id -> subscriberId, 'plan_id -> planId).apply.head
      count[Long]("c")
    }
  }

  def inPlan(subscriberId: Int, planId: Int, page: Int): List[Interest] = {
      val offset = page * 10
      DB.withConnection { implicit c =>
      SQL(
        """
          select interest.* from interest, scheme, plan_scheme
          where interest.subscriber_id = {sub_id}
          and interest.scheme_tag = scheme.tag
          and plan_scheme.scheme_id = scheme.id
          and plan_scheme.plan_id = {plan_id}
          order by interest.id
          limit 10 offset {offset}
        """
      ).on('sub_id -> subscriberId, 'plan_id -> planId, 'offset -> offset).as(interest *)
    }
  }

  // todo: maybe a better name would be schemeInterestCount? The current name makes me expect a count
  // of schemes... which would always be 1 as we pass in the Scheme to count.
  def matchCount(subscriberId: Int, mType: String) = {
    DB.withConnection { implicit c =>
      val count = if (mType == "sub") SQL(
        """
          select count(*) as c from interest
          where subscriber_id = {sub_id}
          and exists (select 1 from interest_subscription where interest_id = interest.id)
        """
        ).on('sub_id -> subscriberId).apply.head
        else SQL(
          """
            select count(*) as c from interest
            where subscriber_id = {sub_id}
            and not exists (select 1 from interest_subscription where interest_id = interest.id)
          """
          ).on('sub_id -> subscriberId).apply.head
      count[Long]("c")
    }
  }

  def inMatch(subscriberId: Int, mType: String, page: Int): List[Interest] = {
      val offset = page * 10
      DB.withConnection { implicit c =>
      if (mType == "sub") SQL(
        """
          select * from interest
          where subscriber_id = {sub_id}
          and exists (select 1 from interest_subscription where interest_id = interest.id)
          order by id
          limit 10 offset {offset}
        """
      ).on('sub_id -> subscriberId, 'offset -> offset).as(interest *)
     else SQL(
       """
         select * from interest
         where subscriber_id = {sub_id}
         and not exists (select 1 from interest_subscription where interest_id = interest.id)
         order by id
         limit 10 offset {offset}
       """
     ).on('sub_id -> subscriberId, 'offset -> offset).as(interest *)
    }
  }

  def unmatched(schemeTag: String): List[Interest] = {
    DB.withConnection { implicit c =>
      SQL(
        """
          select * from interest
          where template = 'false' and scheme_tag = {stag}
          and not exists (select 1 from interest_subscription where interest_id = interest.id)
        """
      ).on('stag -> schemeTag).as(interest *)
    }
  }

  def templates(schemeTag: String): List[Interest] = {
    DB.withConnection { implicit c =>
      SQL("select * from interest where template = 'true' and scheme_tag = {stag}")
      .on('stag -> schemeTag).as(interest *)
    }
  }

  def create(subscriberId: Int, schemeTag: String, intValue: String, template: Boolean) = {
    val created = new Date
    DB.withConnection { implicit c =>
      SQL(
        """
        insert into interest (subscriber_id, scheme_tag, int_value, template, created)
        values ({subscriber_id}, {scheme_tag}, {int_value}, {template}, {created})
        """
      ).on('subscriber_id -> subscriberId, 'scheme_tag -> schemeTag, 'int_value -> intValue, 'template -> template, 'created -> created).executeInsert()
    }
  }

  def make(subscriberId: Int, schemeTag: String, intValue: String, template: Boolean): Interest = {
    findById(create(subscriberId, schemeTag, intValue, template).get.toInt).get
  }

  def delete(id: Int) {
    DB.withConnection { implicit c =>
      SQL("delete from interest_subscription where interest_id = {id}").on('id -> id).executeUpdate()
      SQL("delete from interest where id = {id}").on('id -> id).executeUpdate()
    }
  }
}
