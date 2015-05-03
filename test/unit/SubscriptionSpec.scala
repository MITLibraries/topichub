import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._

import models.Collection
import models.ContentType
import models.Interest
import models.Item
import models.Publisher
import models.ResourceMap
import models.Scheme
import models.Subscriber
import models.Subscription
import models.Topic
import models.Transfer
import models.User

class SubscriptionSpec extends Specification {

  "Subscription model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        Subscription.create(sub.id, t.id, "deliver", sub.created, sub.created)
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(1)
        Subscription.findById(1).get.action must equalTo("deliver")
      }
    }

    "#create does not allow duplicate Subscriptions" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        Subscription.create(sub.id, t.id, "deliver", sub.created, sub.created) must throwA[Exception].like {
                case e: Exception => e.getMessage aka "error" mustEqual (
                  "JdbcSQLException(Unique index or primary key violation)")}
      }.pendingUntilFixed("https://github.com/MITLibraries/scoap3hub/issues/140")
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        val s = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(1)
        s must equalTo(Subscription.findById(1).get)
        s.action must equalTo("deliver")
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        val s = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        Subscription.create(sub.id, t.id, "deliver", sub.created, sub.created)
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(2)
        Subscription.findById(1).get must equalTo(s)
      }
    }

    "#withSubscriber" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(u.id, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        val t2 = Topic.make(scheme.id, "tag2", "name")

        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        val s = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        val s2 = Subscription.make(sub.id, t2.id, "deliver", sub.created, sub.created)
        val s3 = Subscription.make(sub2.id, t.id, "deliver", sub.created, sub.created)
        val subscriptList = Subscription.withSubscriber(s.id)
        subscriptList.size should equalTo(2)
        subscriptList.contains(s) must equalTo(true)
        subscriptList.contains(s2) must equalTo(true)
        subscriptList.contains(s3) must equalTo(false)
      }
    }

    "#withSubscriberAndTopic" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(u.id, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        val t2 = Topic.make(scheme.id, "tag2", "name")
        val t3 = Topic.make(scheme.id, "tag3", "name")

        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        val s1t1 = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        val s1t2 = Subscription.make(sub.id, t2.id, "deliver", sub.created, sub.created)
        val s2t1 = Subscription.make(sub2.id, t.id, "deliver", sub.created, sub.created)
        val s2t2 = Subscription.make(sub2.id, t2.id, "deliver", sub.created, sub.created)
        val subscriptList = Subscription.withSubscriberAndTopic(s1t1.id, t.id)
        subscriptList.size should equalTo(1)
        subscriptList.contains(s1t1) must equalTo(true)
        subscriptList.contains(s1t2) must equalTo(false)
        subscriptList.contains(s2t1) must equalTo(false)
        subscriptList.contains(s2t2) must equalTo(false)
      }
    }

    "#schemeCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(u.id, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val scheme2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))

        val t = Topic.make(scheme.id, "tag", "name")
        val t2 = Topic.make(scheme2.id, "tag2", "name")
        val t3 = Topic.make(scheme2.id, "tag3", "name")
        val t4 = Topic.make(scheme.id, "tag4", "name")
        val t5 = Topic.make(scheme.id, "tag5", "name")

        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)

        // Multiple Subscriptions for topics in one Scheme are only counted once
        val s1t1 = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        val s1t2 = Subscription.make(sub.id, t2.id, "deliver", sub.created, sub.created)
        val s1t3 = Subscription.make(sub.id, t3.id, "deliver", sub.created, sub.created)
        Subscription.schemeCount(sub.id, scheme.id) mustEqual(1)

        // Additional Subscriptions for topics in different Schemes are each counted
        val s1t4 = Subscription.make(sub.id, t4.id, "deliver", sub.created, sub.created)
        val s1t5 = Subscription.make(sub.id, t5.id, "deliver", sub.created, sub.created)
        Subscription.schemeCount(sub.id, scheme.id) mustEqual(3)

        // Subscriptions for different Subscribers are not counted
        val s2t1 = Subscription.make(sub2.id, t.id, "deliver", sub.created, sub.created)
        val s2t2 = Subscription.make(sub2.id, t2.id, "deliver", sub.created, sub.created)
        Subscription.schemeCount(sub.id, scheme.id) mustEqual(3)
      }
    }

    "#inScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(u.id, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val scheme2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))

        val t = Topic.make(scheme.id, "tag", "name")
        val t2 = Topic.make(scheme2.id, "tag2", "name")
        val t3 = Topic.make(scheme2.id, "tag3", "name")
        val t4 = Topic.make(scheme.id, "tag4", "name")
        val t5 = Topic.make(scheme.id, "tag5", "name")

        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)

        // Three subscriptions to Topics in the same Schemes
        val s1t1 = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        val s1t4 = Subscription.make(sub.id, t4.id, "deliver", sub.created, sub.created)
        val s1t5 = Subscription.make(sub.id, t5.id, "deliver", sub.created, sub.created)

        // Additional Subscriptions for topics in different Schemes
        val s1t2 = Subscription.make(sub.id, t2.id, "deliver", sub.created, sub.created)
        val s1t3 = Subscription.make(sub.id, t3.id, "deliver", sub.created, sub.created)

        // Subscriptions for different Subscribers to initial Schemes
        val s2t1 = Subscription.make(sub2.id, t.id, "deliver", sub.created, sub.created)
        val s2t2 = Subscription.make(sub2.id, t5.id, "deliver", sub.created, sub.created)

        val inScheme = Subscription.inScheme(sub.id, scheme.id, 0)
        inScheme.size mustEqual(3)
        inScheme.contains(s1t1) mustEqual(true)
        inScheme.contains(s1t2) mustEqual(false)
        inScheme.contains(s1t3) mustEqual(false)
        inScheme.contains(s1t4) mustEqual(true)
        inScheme.contains(s1t5) mustEqual(true)
        inScheme.contains(s2t1) mustEqual(false)
        inScheme.contains(s2t2) mustEqual(false)
      }
    }

    "#cancel" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        val s = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        s.active mustEqual(true)
        s.cancel
        Subscription.findById(s.id).get.active mustEqual(false)
      }
    }

    "#topic" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        val s = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        s.topic mustEqual(t)
      }
    }

    "#subscriber" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        val s = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        s.subscriber mustEqual(sub)
      }
    }

    "#transferCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag", "name")
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        val s = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        s.transferCount mustEqual(0)

        val pub = Publisher.make(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        val item2 = Item.make(col.id, ct.id, "location", "scoap:abc:456")
        val item3 = Item.make(col.id, ct.id, "location", "scoap:abc:789")

        Transfer.create(sub.id, s.id, item.id, "review")
        Transfer.create(sub.id, s.id, item2.id, "deliver")
        Transfer.create(sub.id, s.id, item3.id, "review")
        Subscription.findById(s.id).get.transferCount mustEqual(3)
      }
    }

    "#linkInterest" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        val t = Topic.make(scheme.id, "tag", "name")
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        val s = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        Interest.matchCount(sub.id, "sub") must equalTo(0)
        s.linkInterest(i)
        Interest.matchCount(sub.id, "sub") must equalTo(1)
      }
    }

    "#unlinkInterest" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        val t = Topic.make(scheme.id, "tag", "name")
        Subscription.schemeCount(sub.id, scheme.id) must equalTo(0)
        val s = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        s.linkInterest(i)
        Interest.matchCount(sub.id, "sub") must equalTo(1)
        s.unlinkInterest
        Interest.matchCount(sub.id, "sub") must equalTo(0)
      }
    }
  }
}
