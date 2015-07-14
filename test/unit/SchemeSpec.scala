import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import models.{ContentType, Collection, Item, Publisher, ResourceMap, Scheme, Topic, User, Validator}
import java.util.Date

class SchemeSpec extends Specification {

  "Scheme model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.all must haveSize(1)
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.all must haveSize(0)
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.all must haveSize(1)
        s.tag must equalTo("tag")
      }
    }

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.all must haveSize(2)
        Scheme.all.contains(Scheme.findById(1).get) must equalTo(true)
        Scheme.all.contains(Scheme.findById(2).get) must equalTo(true)
      }
    }

    "#withGentype" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype2", "cat", "desc", Some("link"), Some("logo"))
        Scheme.withGentype("gentype") must haveSize(2)
        Scheme.withGentype("gentype").contains(Scheme.findById(1).get) must equalTo(true)
        Scheme.withGentype("gentype").contains(Scheme.findById(2).get) must equalTo(true)
        Scheme.withGentype("gentype").contains(Scheme.findById(3).get) must equalTo(false)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.findById(1).get.tag must equalTo("tag")
      }
    }

    "#findByTag" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.findByTag("tag").get.id must equalTo(1)
      }
    }

    "#mapView" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype2", "cat", "desc", Some("link"), Some("logo"))
        Scheme.mapView must havePair("1" -> "tag")
        Scheme.mapView must havePair("2" -> "tag2")
        Scheme.mapView must havePair("3" -> "tag3")
      }
    }

    "#gentypeMapView" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype2", "cat", "desc", Some("link"), Some("logo"))
        Scheme.gentypeMapView("gentype") must havePair("1" -> "tag")
        Scheme.gentypeMapView("gentype") must havePair("2" -> "tag2")
        Scheme.gentypeMapView("gentype") must not havePair("3" -> "tag3")
      }
    }

    "#topicCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.findById(1).get.topicCount must equalTo(0)
        Scheme.findById(2).get.topicCount must equalTo(0)
        Topic.create(1, "tag", "meta")
        Topic.create(1, "tag2", "meta")
        Scheme.findById(1).get.topicCount must equalTo(2)
        Scheme.findById(2).get.topicCount must equalTo(0)
        Topic.create(2, "tag2", "meta")
        Scheme.findById(1).get.topicCount must equalTo(2)
        Scheme.findById(2).get.topicCount must equalTo(1)
      }
    }

    "#itemCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.findById(1).get.itemCount must equalTo(0)
        Scheme.findById(2).get.itemCount must equalTo(0)
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val pub = Publisher.make(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        val topic = Topic.make(1, "tag", "meta")
        item.addTopic(topic)
        Scheme.findById(1).get.itemCount must equalTo(1)
        Scheme.findById(2).get.itemCount must equalTo(0)
      }
    }

    "#validator" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.findById(1).get.validator must equalTo(None)
        Validator.create(1, "some validator", "user", "pwd", "code", "http://www.example.com", "notMe")
        Scheme.findById(1).get.validator must equalTo(Some(Validator.findByScheme(1).head))
      }
    }
  }
}
