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

/** Item represents a distinct content aggregation, typically containing
  * a prinary artifcat, and metadata or other auxillary files. While opaque
  * in the data model, the ResourceMap entity is used to characterize it.
  *
  * @author richardrodgers
  */

case class Item(id: Int,            // DB key
                collectionId: Int,  // Owning collection ID
                ctypeId: Int,       // Content type ID
                location: String,   // Content storage location
                objKey: String,     // location-relative key/id/name
                created: Date,      // When Item added to hub
                updated: Date,      // Time of last transfer
                transfers: Int)  {  // Number of transfers

  def addTopic(topic: Topic) {
    DB.withConnection { implicit c =>
      SQL("insert into item_topic (item_id, item_created, topic_id) values ({item_id}, {item_created}, {topic_id})")
      .on('item_id -> id, 'item_created -> created, 'topic_id -> topic.id).executeUpdate()
    }
  }

  def topics = {
    DB.withConnection { implicit c =>
      SQL("select topic.* from topic, item_topic where topic.id = item_topic.topic_id and item_topic.item_id = {item_id}")
      .on('item_id -> id).as(Topic.topic *)
    }
  }

  def regularTopics = {
    DB.withConnection { implicit c =>
      SQL("select topic.* from topic, item_topic, scheme where topic.id = item_topic.topic_id and item_topic.item_id = {item_id} and topic.scheme_id = scheme.id and scheme.tag != 'meta'")
      .on('item_id -> id).as(Topic.topic *).groupBy(_.scheme_id).map { el =>
        (Scheme.findById(el._1).get.tag, el._2)
      }
    }
  }

  def contentType = {
    DB.withConnection { implicit c =>
      SQL("select * from content_type where id = {ctype_id}")
      .on('ctype_id -> ctypeId).as(ContentType.ctype.singleOpt)
    }
  }

  def addMetadata(mdname: String, mdvalue: String) {
    DB.withConnection { implicit c =>
      SQL("insert into metadata (item_id, mdname, mdvalue) values ({item_id}, {mdname}, {mdvalue})")
      .on('item_id -> id, 'mdname -> mdname, 'mdvalue -> mdvalue).executeUpdate()
    }
  }

  def metadataValue(mdname: String) = {
    DB.withConnection { implicit c =>
      SQL("select mdvalue from metadata where item_id = {item_id} and mdname = {mdname}")
      .on('item_id -> id, 'mdname -> mdname).apply().headOption match {
        case Some(x) => x[String]("mdvalue")
        case None => "Unknown Value"
      }
    }
  }

  def hasMetadata(mdname: String) = {
    DB.withConnection { implicit c =>
      SQL("select mdvalue from metadata where item_id = {item_id} and mdname = {mdname}")
      .on('item_id -> id, 'mdname -> mdname).apply().headOption match {
        case Some(x) => true
        case None => false
      }
    }
  }

  def metadataValues(mdname: String) = {
    DB.withConnection { implicit c =>
      val rows = SQL("select mdvalue from metadata where item_id = {item_id} and mdname = {mdname}")
      .on('item_id -> id, 'mdname -> mdname)
      rows().map(row => row[String]("mdvalue")).toList
    }
  }
}

object Item {

  val item = {
    get[Int]("id") ~ get[Int]("collection_id") ~ get[Int]("content_type_id") ~ get[String]("location") ~
    get[String]("obj_key") ~ get[Date]("created") ~ get[Date]("updated") ~ get[Int]("transfers") map {
      case id ~ collectionId ~ ctypeId ~ location ~ objKey ~ created ~ updated ~ transfers =>
        Item(id, collectionId, ctypeId, location, objKey, created, updated, transfers)
    }
  }

  def create(collectionId: Int, ctypeId: Int, location: String, objKey: String) = {
    DB.withConnection { implicit c =>
      SQL("insert into item (collection_id, content_type_id, location, obj_key, created, updated, transfers) values ({collection_id}, {content_type_id}, {location}, {obj_key}, {created}, {updated}, {transfers})")
      .on('collection_id -> collectionId, 'content_type_id -> ctypeId, 'location -> location, 'obj_key -> objKey, 'created -> new Date, 'updated -> new Date, 'transfers -> 0).executeInsert()
    }
  }

  def make(collectionId: Int, ctypeId: Int, location: String, objKey: String): Item = {
    findById(create(collectionId, ctypeId, location, objKey).get.toInt).get
  }

  def findById(id: Int): Option[Item] = {
    DB.withConnection { implicit c =>
      SQL("select * from item where id = {id}").on('id -> id).as(item.singleOpt)
    }
  }

  def all: List[Item] = {
    DB.withConnection { implicit c =>
      SQL("select * from item").as(item *)
    }
  }

  def findByKey(key: String): Option[Item] = {
    DB.withConnection { implicit c =>
      SQL("select * from item where obj_key = {key}").on('key -> key).as(item.singleOpt)
    }
  }

  def inCollection(coll_id: Int, page: Int): List[Item] = {
    val offset = page * 10
    DB.withConnection { implicit c =>
      SQL(
        """
          select * from item where collection_id = {id}
          order by created desc
          limit 10 offset {offset}
        """
      ).on('id -> coll_id, 'offset -> offset).as(item *)
    }
  }

  def collectionCount(coll_id: Int) = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from item where collection_id = {id}").on('id -> coll_id).apply.head
      count[Long]("c")
    }
  }

  def delete(id: Int) {
    DB.withConnection { implicit c =>
      SQL("delete from item_topic where item_id = {item_id}").on('item_id -> id).executeUpdate()
      SQL("delete from metadata where item_id = {item_id}").on('item_id -> id).executeUpdate()
      SQL("delete from item where id = {id}").on('id -> id).executeUpdate()
    }
  }

  def deleteBefore(date: Date) {
    DB.withConnection { implicit c =>
      val rows = SQL("select id from item where created < {created}").on('created -> date)
      rows().map(row => row[Int]("id")).toList.foreach(delete)
    }
  }
}
