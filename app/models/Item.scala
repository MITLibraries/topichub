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

  def hasTopic(topic: Topic): Boolean = {
    DB.withConnection { implicit c =>
      val count = SQL("select count(*) as c from item_topic where topic_id = {topic_id} and item_id = {item_id}")
      .on('topic_id -> topic.id, 'item_id -> id).apply.head
      count[Long]("c") > 0
    }
  }

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

  def filename(uri: String) = {
    if( uri.endsWith("pdf") && hasMetadata("doi") ) {
      metadataValue("doi").split("/").last + ".pdf"
    } else if( uri.endsWith("pdfa") && hasMetadata("doi") ) {
      metadataValue("doi").split("/").last + "_pdfa.pdf"
    } else if( uri.endsWith("xml") && hasMetadata("doi") ) {
      metadataValue("doi").split("/").last + ".xml"
    } else {
      "filename_error"
    }
  }

  def toMets = {
    <mets xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xmlns="http://www.loc.gov/METS/"
      xmlns:xlink="http://www.w3.org/1999/xlink"
      xsi:schemaLocation="http://www.loc.gov/METS/ http://www.loc.gov/standards/mets/mets.xsd"
      OBJID="sword-mets"
      LABEL="DSpace SWORD Item"
      PROFILE="DSpace METS SIP Profile 1.0">

      {metsHdr}
      {mets_dmdSec}
      {mets_fileSec}
      {mets_structMap}
    </mets>
  }

  private def metsHdr = {
    <metsHdr CREATEDATE={metadataValue("title")}>
      <agent ROLE="CREATOR" TYPE="ORGANIZATION">
        <name>TopicHub</name>
      </agent>
    </metsHdr>
  }

  private def mets_dmdSec = {
    <dmdSec ID="sword-mets-dmd-1" GROUPID="sword-mets-dmd-1_group-1">
        <mdWrap LABEL="SWAP Metadata" MDTYPE="OTHER" OTHERMDTYPE="EPDCX" MIMETYPE="text/xml">
          <xmlData>
            <epdcx:descriptionSet xmlns:epdcx="http://purl.org/eprint/epdcx/2006-11-16/" xmlns:MIOJAVI="http://purl.org/eprint/epdcx/2006-11-16/" xsi:schemaLocation="http://purl.org/eprint/epdcx/2006-11-16/ http://purl.org/eprint/epdcx/xsd/2006-11-16/epdcx.xsd">
              <epdcx:description epdcx:resourceId="sword-mets-epdcx-1">
                <epdcx:statement epdcx:propertyURI="http://purl.org/dc/elements/1.1/type" epdcx:valueURI="http://purl.org/eprint/entityType/ScholarlyWork"/>

                { if( hasMetadata("title") )
                  <epdcx:statement epdcx:propertyURI="http://purl.org/dc/elements/1.1/title">
                    <epdcx:valueString>{ metadataValue("title") }</epdcx:valueString>
                  </epdcx:statement>
                }

                { if( hasMetadata("author") )
                  { for ( author <- metadataValues("author") ) yield
                    <epdcx:statement epdcx:propertyURI="http://purl.org/dc/elements/1.1/creator">
                      <epdcx:valueString>{ author }</epdcx:valueString>
                    </epdcx:statement>
                  }
                }

                { if( hasMetadata("additional_author") )
                  { for ( author <- metadataValues("additional_author") ) yield
                    <epdcx:statement epdcx:propertyURI="http://purl.org/dc/elements/1.1/creator">
                      <epdcx:valueString>{ author }</epdcx:valueString>
                    </epdcx:statement>
                  }
                }

                { if( hasMetadata("abstract") )
                  <epdcx:statement epdcx:propertyURI="http://purl.org/dc/terms/abstract">
                    <epdcx:valueString>{ metadataValue("abstract") }</epdcx:valueString>
                  </epdcx:statement>
                }

                <epdcx:statement epdcx:propertyURI="http://purl.org/eprint/terms/isExpressedAs" epdcx:valueRef="sword-mets-expr-1"/>
              </epdcx:description>

              <epdcx:description epdcx:resourceId="sword-mets-expr-1">
                <epdcx:statement epdcx:propertyURI="http://purl.org/dc/elements/1.1/type" epdcx:valueURI="http://purl.org/eprint/entityType/Expression"/>

                <epdcx:statement epdcx:propertyURI="http://purl.org/dc/elements/1.1/type" epdcx:vesURI="http://purl.org/eprint/terms/Type" epdcx:valueURI="http://purl.org/eprint/type/JournalArticle"/>

                { if( hasMetadata("publisher") )
                  <epdcx:statement epdcx:propertyURI="http://purl.org/dc/elements/1.1/publisher">
                  <epdcx:valueString>{ metadataValue("publisher") }</epdcx:valueString>
                </epdcx:statement>
                }

                { if( hasMetadata("copyright_uri") )
                  <epdcx:statement epdcx:propertyURI="http://purl.org/dc/terms/license">
                    <epdcx:valueString>{ metadataValue("copyright_uri") }</epdcx:valueString>
                  </epdcx:statement>
                }

                { if( hasMetadata("copyright_holder") )
                  <epdcx:statement epdcx:propertyURI="http://purl.org/eprint/terms/copyrightHolder">
                    <epdcx:valueString>{ metadataValue("copyright_holder") }</epdcx:valueString>
                  </epdcx:statement>
                }

                { if( hasMetadata("doi") )
                  <epdcx:statement epdcx:propertyURI="http://purl.org/dc/elements/1.1/identifier">
                    <epdcx:valueString epdcx:sesURI="http://purl.org/dc/terms/URI">
                      http://dx.doi.org/{ metadataValue("doi") }
                    </epdcx:valueString>
                  </epdcx:statement>
                }

              </epdcx:description>
            </epdcx:descriptionSet>
          </xmlData>
        </mdWrap>
      </dmdSec>
  }

  private def mets_fileSec = {
    <fileSec>
      <fileGrp ID="sword-mets-fgrp-1" USE="CONTENT">
        { for ( uri <- metadataValues("accessUri") ) yield
          if( uri.endsWith("pdf") || uri.endsWith("pdfa") ) {
            <file ID={ filename(uri) } MIMETYPE="application/pdf">
              <FLocat LOCTYPE="URL" xlink:href={ filename(uri) } />
            </file>
          } else if( uri.endsWith("xml") ) {
            <file ID={ filename(uri) } MIMETYPE="text/xml">
              <FLocat LOCTYPE="URL" xlink:href={ filename(uri) } />
            </file>
          }
        }
      </fileGrp>
    </fileSec>
  }

  private def mets_structMap = {
    <structMap ID="sword-mets-struct-1" LABEL="structure" TYPE="LOGICAL">
      <div ID="sword-mets-div-1" DMDID="sword-mets-dmd-1" TYPE="SWORD Object">

      { for( uri <- metadataValues("accessUri") ) yield
        <div TYPE="File">
          <fptr FILEID={ filename(uri) }/>
        </div>
      }

      </div>
    </structMap>
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

  def allMissingMetadata: List[Item] = {
    DB.withConnection { implicit c =>
      SQL("""
              SELECT item.*
              FROM item
              LEFT JOIN item_topic on item.id = item_topic.item_id
              WHERE item_topic.id IS NULL
              ORDER BY created DESC
          """).as(item *)
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
      val rows = SQL("select id from item where created <= {created}").on('created -> date)
      rows().map(row => row[Int]("id")).toList.foreach(delete)
    }
  }
}
