import org.specs2.mutable._
import java.util.Date

import play.api.test._
import play.api.test.Helpers._
import models.Collection
import models.ContentType
import models.Hold
import models.Item
import models.Publisher
import models.ResourceMap
import models.Scheme
import models.Subscriber
import models.Subscription
import models.Topic
import models.User

class HoldSpec extends Specification {

  "Hold model" should {

    "#held" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val subscr = Subscription.make(s.id, top.id, "review", new Date(0), new Date(1000000000))
        val pub = Publisher.make(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        Hold.held(item.id, s.id) must equalTo(false)
        val h = Hold.make(s.id, subscr.id, item.id)
        Hold.held(item.id, s.id) must equalTo(true)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val subscr = Subscription.make(s.id, top.id, "review", new Date(0), new Date(1000000000))
        val pub = Publisher.make(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        Hold.held(item.id, s.id) must equalTo(false)
        val h = Hold.make(s.id, subscr.id, item.id)
        Hold.findById(1).get must equalTo(h)
      }
    }

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val subscr = Subscription.make(s.id, top.id, "review", new Date(0), new Date(1000000000))
        val pub = Publisher.make(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        Hold.held(item.id, s.id) must equalTo(false)
        Hold.create(s.id, subscr.id, item.id)
        Hold.held(item.id, s.id) must equalTo(true)
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val subscr = Subscription.make(s.id, top.id, "review", new Date(0), new Date(1000000000))
        val pub = Publisher.make(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        Hold.held(item.id, s.id) must equalTo(false)
        val h = Hold.make(s.id, subscr.id, item.id)
        Hold.held(item.id, s.id) must equalTo(true)
      }
    }

    "#item" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val subscr = Subscription.make(s.id, top.id, "review", new Date(0), new Date(1000000000))
        val pub = Publisher.make(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        val h = Hold.make(s.id, subscr.id, item.id)
        h.item must equalTo(item)
      }
    }

    "#subscription" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val subscr = Subscription.make(s.id, top.id, "review", new Date(0), new Date(1000000000))
        val pub = Publisher.make(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        val h = Hold.make(s.id, subscr.id, item.id)
        h.subscription must equalTo(subscr)
      }
    }

    "#resolve" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val subscr = Subscription.make(s.id, top.id, "review", new Date(0), new Date(1000000000))
        val pub = Publisher.make(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        val h = Hold.make(s.id, subscr.id, item.id)
        Hold.held(item.id, s.id) must equalTo(true)
        h.resolve(true)
        Hold.held(item.id, s.id) must equalTo(false)

        val h2 = Hold.make(s.id, subscr.id, item.id)
        Hold.held(item.id, s.id) must equalTo(true)
        h2.resolve(false)
        Hold.held(item.id, s.id) must equalTo(false)
      }
    }
  }
}
