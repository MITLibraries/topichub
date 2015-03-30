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

case class User(id: Int, name: String, email: String, password: String,
                role: String, created: Date, accessed: Date) {

  def hasPublisher(pub_id: Int): Boolean = {
    DB.withConnection { implicit c =>
      SQL("select * from publisher where id = {pub_id} and hub_user_id = {hub_user_id}").on('pub_id -> pub_id, 'hub_user_id -> id).as(Publisher.pub.singleOpt).isDefined
    }
  }
}

object User {

  val user = {
    get[Int]("id") ~ get[String]("name") ~ get[String]("email") ~ get[String]("password") ~
    get[String]("role") ~ get[Date]("created") ~ get[Date]("accessed") map {
      case id ~ name ~ email ~ password ~ role ~ created ~ accessed =>
        User(id, name, email, password, role, created, accessed)
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

  def create(name: String, email: String, password: String, role: String) = {
    DB.withConnection { implicit c =>
      SQL("insert into hub_user (name, email, password, role, created, accessed) values ({name}, {email}, {password}, {role}, {created}, {accessed})")
      .on('name -> name, 'email -> email, 'password -> password, 'role -> role, 'created -> new Date, 'accessed -> new Date).executeInsert()
    }
  }

  def make(name: String, email: String, password: String, role: String): User = {
    findById(create(name, email, password, role).get.toInt).get
  }

}
