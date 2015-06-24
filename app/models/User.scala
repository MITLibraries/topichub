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

/** User is an authenticated identity to a hub, who may be a publisher, subscriber, editor,
  * site administrator or some combination thereof.
  *
  * @author richardrodgers
  */

case class User(id: Int, name: String, email: String,
                role: String, created: Date, accessed: Date, identity: String) {

  def hasPublisher(pub_id: Int): Boolean = {
    DB.withConnection { implicit c =>
      SQL("select * from publisher where id = {pub_id} and hub_user_id = {hub_user_id}").on('pub_id -> pub_id, 'hub_user_id -> id).as(Publisher.pub.singleOpt).isDefined
    }
  }

  def hasRole(role: String): Boolean = {
    DB.withConnection { implicit c =>
      SQL("""SELECT COUNT(*) as c FROM hub_user
             WHERE id = {id}
             AND role LIKE {role}""")
          .on('id -> id, 'role -> ("%"+role+"%")).as(scalar[Long].single) > 0
    }
  }
}

object User {

  val user = {
    get[Int]("id") ~ get[String]("name") ~ get[String]("email") ~
    get[String]("role") ~ get[Date]("created") ~ get[Date]("accessed") ~ get[String]("identity") map {
      case id ~ name ~ email ~ role ~ created ~ accessed ~ identity =>
        User(id, name, email, role, created, accessed, identity)
    }
  }

  def findById(id: Int): Option[User] = {
    DB.withConnection { implicit c =>
      SQL("select * from hub_user where id = {id}").on('id -> id).as(user.singleOpt)
    }
  }

  def findByName(name: String): Option[User] = {
    DB.withConnection { implicit c =>
      SQL("select * from hub_user where name = {name}").on('name -> name).as(user.singleOpt)
    }
  }

  def allByRole(role: String): List[User] = {
    DB.withConnection { implicit c =>
      SQL("""
          SELECT * FROM hub_user
          WHERE role LIKE {role}
          """).on('role -> ("%"+role+"%")).as(user *)
    }
  }

  def create(name: String, email: String, role: String, identity: String) = {
    DB.withConnection { implicit c =>
      SQL("insert into hub_user (name, email, role, created, accessed, identity) values ({name}, {email}, {role}, {created}, {accessed}, {identity})")
      .on('name -> name, 'email -> email, 'role -> role, 'created -> new Date, 'accessed -> new Date, 'identity -> identity).executeInsert()
    }
  }

  def make(name: String, email: String, role: String, identity: String): User = {
    findById(create(name, email, role, identity).get.toInt).get
  }

  def findByIdentity(identity: String): Option[User] = {
    DB.withConnection { implicit c =>
      SQL("select * from hub_user where identity = {identity}").on('identity -> identity).as(user.singleOpt)
    }
  }

  def isValidIdentity(identity: String): Boolean = {
    DB.withConnection { implicit c =>
      SQL("select count(*) from hub_user where identity = {identity}").on('identity -> identity).as(scalar[Long].single) > 0
    }
  }
}
