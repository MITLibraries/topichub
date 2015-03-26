import org.specs2.mutable._
import java.util.Date

import play.api.test._
import play.api.test.Helpers._
import models.Collection
import models.ContentType
import models.Item
import models.Publisher
import models.ResourceMap
import models.Scheme
import models.Subscriber
import models.Subscription
import models.Topic
import models.Transfer
import models.User
import models.Validator
import java.util.Date

class TransferSpec extends Specification {

  "Transfer model" should {

    "#transferred" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val subscriber = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val subscriber2 = Subscriber.make(u.id, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))

        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        val subscription = Subscription.make(subscriber.id, t.id, "deliver", subscriber.created, subscriber.created)
        val pub = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item1 = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        val item2 = Item.make(col.id, ct.id, "location", "scoap:abc:456")
        val item3 = Item.make(col.id, ct.id, "location", "scoap:abc:789")


        Transfer.findById(1) must equalTo(None)
        val tran = Transfer.make(subscriber.id, subscription.id, item1.id, "doit")
        val tran2 = Transfer.make(subscriber.id, subscription.id, item2.id, "doit")

        Transfer.transferred(item1.id, subscriber.id) must equalTo(true)
        Transfer.transferred(item2.id, subscriber.id) must equalTo(true)

        Transfer.transferred(item1.id, subscriber2.id) must equalTo(false)
        Transfer.transferred(item2.id, subscriber2.id) must equalTo(false)

        Transfer.transferred(item3.id, subscriber.id) must equalTo(false)
        Transfer.transferred(item3.id, subscriber2.id) must equalTo(false)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val subscriber = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        val subscription = Subscription.make(subscriber.id, t.id, "deliver", subscriber.created, subscriber.created)
        val pub = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        val item2 = Item.make(col.id, ct.id, "location", "scoap:abc:456")

        Transfer.findById(1) must equalTo(None)
        val tran = Transfer.make(subscriber.id, subscription.id, item.id, "doit")
        val tran2 = Transfer.make(subscriber.id, subscription.id, item2.id, "doit")
        Transfer.findById(tran.id) must equalTo(Some(tran))
        Transfer.findById(tran2.id) must equalTo(Some(tran2))
      }
    }

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val subscriber = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        val subscription = Subscription.make(subscriber.id, t.id, "deliver", subscriber.created, subscriber.created)
        val pub = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")

        Transfer.findById(1) must equalTo(None)
        Transfer.create(subscriber.id, subscription.id, item.id, "doit")
        Transfer.findById(1) must not equalTo(None)
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val subscriber = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        val subscription = Subscription.make(subscriber.id, t.id, "deliver", subscriber.created, subscriber.created)
        val pub = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")

        Transfer.findById(1) must equalTo(None)
        val tran = Transfer.make(subscriber.id, subscription.id, item.id, "doit")
        Transfer.findById(tran.id) must equalTo(Some(tran))
      }
    }
  }
}
