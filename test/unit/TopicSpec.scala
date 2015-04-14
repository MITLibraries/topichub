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
import models.User
import models.Validator
import java.util.Date

class TopicSpec extends Specification {

  "Topic model" should {

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "tag", "name")
        Topic.all must haveSize(1)
        Topic.findById(t1.id).get must equalTo(t1)
      }
    }

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "tag", "name")
        val t2 = Topic.make(s.id, "tag2", "name2")
        Topic.all must haveSize(2)
        Topic.all.contains(t1) must equalTo(true)
        Topic.all.contains(t2) must equalTo(true)
      }
    }

    "#withScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "tag01", "name")
        val t2 = Topic.make(s.id, "tag02", "name2")
        val t3 = Topic.make(s2.id, "other1", "name3")
        Topic.all must haveSize(3)

        val topicsScheme1 = Topic.withScheme(s.id, 0)
        topicsScheme1 must haveSize(2)
        topicsScheme1.contains(t1) must equalTo(true)
        topicsScheme1.contains(t2) must equalTo(true)
        topicsScheme1.contains(t3) must equalTo(false)
        Topic.withScheme(s2.id, 0).contains(t3) must equalTo(true)

        //pagination
        val t4 = Topic.make(s.id, "tag03", "name2")
        val t5 = Topic.make(s.id, "tag04", "name2")
        val t6 = Topic.make(s.id, "tag05", "name2")
        val t7 = Topic.make(s.id, "tag06", "name2")
        val t8 = Topic.make(s.id, "tag07", "name2")
        val t9 = Topic.make(s.id, "tag08", "name2")
        val t10 = Topic.make(s.id, "tag09", "name2")
        val t11 = Topic.make(s.id, "tag10", "name2")
        val t12 = Topic.make(s.id, "tag11", "name2")

        val topicsScheme1_page1 = Topic.withScheme(s.id, 0)
        topicsScheme1_page1 must haveSize(10)
        topicsScheme1_page1.contains(t1) must equalTo(true)
        //t3 is in a different scheme
        topicsScheme1_page1.contains(t3) must equalTo(false)
        topicsScheme1_page1.contains(t11) must equalTo(true)
        topicsScheme1_page1.contains(t12) must equalTo(false)

        val topicsScheme1_page2 = Topic.withScheme(s.id, 1)
        topicsScheme1_page2 must haveSize(1)
        topicsScheme1_page2.contains(t12) must equalTo(true)

        //page 3 is empty
        Topic.withScheme(1, 2) must haveSize(0)
      }
    }

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(s.id, "tag", "name")
        Topic.all must haveSize(1)
      }
    }

    "#forSchemeAndTag" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "topictag1", "name")
        Topic.forSchemeAndTag("schemetag1", "topictag1") must equalTo(Some(t1))
        Topic.forSchemeAndTag("fakescheme", "topictag1") must equalTo(None)
        Topic.forSchemeAndTag("fakescheme", "faketopic") must equalTo(None)
        Topic.forSchemeAndTag("schemetag1", "faketopic") must equalTo(None)

        val t2 = Topic.make(s.id, "topictag2", "name")
        Topic.forSchemeAndTag("schemetag1", "topictag1") must equalTo(Some(t1))
        Topic.forSchemeAndTag("schemetag1", "topictag2") must equalTo(Some(t2))
      }
    }

    "#deleteUnlinkedBefore with no linked items" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "topictag1", "name")
        val t2 = Topic.make(s.id, "topictag2", "name")
        Topic.all must haveSize(2)

        Thread.sleep(500)
        Topic.deleteUnlinkedBefore(new Date)
        Topic.all must haveSize(0)
      }
    }

    "#deleteUnlinkedBefore with linked items" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {

        Topic.all must haveSize(0)
        val s = Scheme.make("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "topictag1", "name")
        val t2 = Topic.make(s.id, "topictag2", "name")
        Topic.all must haveSize(2)

        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val pub = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        val col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        val item2 = Item.make(col.id, ct.id, "location", "scoap:abc:456")
        val item3 = Item.make(col.id, ct.id, "location", "scoap:abc:789")
        item.addTopic(t1)

        Thread.sleep(500)
        Topic.deleteUnlinkedBefore(new Date)
        Topic.all must haveSize(1)
      }
    }

    "#recentItems" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "topictag1", "name")
        val t2 = Topic.make(s.id, "topictag2", "name")

        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val pub = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        val col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        val item2 = Item.make(col.id, ct.id, "location", "scoap:abc:456")
        val item3 = Item.make(col.id, ct.id, "location", "scoap:abc:789")
        item.addTopic(t1)
        item2.addTopic(t1)

        val recentT1 = Topic.findById(t1.id).get.recentItems(10)
        recentT1.size must equalTo(2)
        recentT1.contains(item) must equalTo(true)
        recentT1.contains(item2) must equalTo(true)
        recentT1.contains(item3) must equalTo(false)

        // requesting smaller max returns appropriate result
        Topic.findById(t1.id).get.recentItems(1).size must equalTo(1)

        // Items for additional topic does not affect first topic
        item3.addTopic(t2)
        Topic.findById(t1.id).get.recentItems(10).size must equalTo(2)
        Topic.findById(t1.id).get.recentItems(1).size must equalTo(1)
      }
    }

    "#itemsSince" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "topictag1", "name")
        val t2 = Topic.make(s.id, "topictag2", "name")

        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val pub = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        val col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        Thread.sleep(500)
        val item2 = Item.make(col.id, ct.id, "location", "scoap:abc:456")
        Thread.sleep(500)
        val item3 = Item.make(col.id, ct.id, "location", "scoap:abc:789")
        item.addTopic(t1)
        item2.addTopic(t1)

        t1.itemsSince(item.created).size must equalTo(2)
        t1.itemsSince(item2.created).size must equalTo(1)
        t1.itemsSince(item3.created).size must equalTo(0)
      }
    }

    "#pagedItems" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "topictag1", "name")
        val t2 = Topic.make(s.id, "topictag2", "name")

        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val pub = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        val col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        Thread.sleep(500)
        val item2 = Item.make(col.id, ct.id, "location", "scoap:abc:456")
        Thread.sleep(500)
        val item3 = Item.make(col.id, ct.id, "location", "scoap:abc:789")
        item.addTopic(t1)
        item2.addTopic(t1)
        item3.addTopic(t2)

        val pageOnePerTen = t1.pagedItems(0, 10)
        pageOnePerTen.size must equalTo(2)
        pageOnePerTen.contains(item) must equalTo(true)
        pageOnePerTen.contains(item2) must equalTo(true)
        pageOnePerTen.contains(item3) must equalTo(false)

        // requesting smaller perpage returns appropriate result
        val pageOnePerOne = t1.pagedItems(0, 1)
        pageOnePerOne.size must equalTo(1)
        pageOnePerOne.contains(item) must equalTo(false)
        pageOnePerOne.contains(item2) must equalTo(true)
        pageOnePerOne.contains(item3) must equalTo(false)

        // requesting smaller perpage returns appropriate result
        val pageTwoPerOne = t1.pagedItems(1, 1)
        pageTwoPerOne.size must equalTo(1)
        pageTwoPerOne.contains(item) must equalTo(true)
        pageTwoPerOne.contains(item2) must equalTo(false)
        pageTwoPerOne.contains(item3) must equalTo(false)
      }
    }

    "#itemCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "topictag1", "name")
        val t2 = Topic.make(s.id, "topictag2", "name")

        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val pub = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        val col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        val item2 = Item.make(col.id, ct.id, "location", "scoap:abc:456")
        val item3 = Item.make(col.id, ct.id, "location", "scoap:abc:789")
        item.addTopic(t1)
        item2.addTopic(t1)
        item3.addTopic(t2)

        t1.itemCount must equalTo(2)
        t2.itemCount must equalTo(1)
      }
    }

    "#itemCountSince" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "topictag1", "name")
        val t2 = Topic.make(s.id, "topictag2", "name")

        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val pub = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        val col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        Thread.sleep(500)
        val item2 = Item.make(col.id, ct.id, "location", "scoap:abc:456")
        Thread.sleep(500)
        val item3 = Item.make(col.id, ct.id, "location", "scoap:abc:789")
        item.addTopic(t1)
        item2.addTopic(t1)
        item3.addTopic(t2)

        t1.itemCountSince(item.created) must equalTo(2)
        t1.itemCountSince(item2.created) must equalTo(1)
        t1.itemCountSince(item3.created) must equalTo(0)
      }
    }

    "#scheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        val s = Scheme.make("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s2 = Scheme.make("schemetag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "topictag1", "name")
        val t2 = Topic.make(s.id, "topictag2", "name2")
        val t3 = Topic.make(s2.id, "topictag3", "name3")

        t1.scheme must equalTo(Some(s))
        t2.scheme must equalTo(Some(s))
        t3.scheme must equalTo(Some(s2))
      }
    }

    "#subscriptionCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(u.id, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))


        Topic.all must haveSize(0)
        val s = Scheme.make("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s2 = Scheme.make("schemetag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "topictag1", "name")
        val t2 = Topic.make(s.id, "topictag2", "name2")
        val t3 = Topic.make(s2.id, "topictag3", "name3")

        t1.subscriptionCount must equalTo(0)
        t2.subscriptionCount must equalTo(0)
        t3.subscriptionCount must equalTo(0)

        // add subs to t1, count increases but does not affect t2 or t3
        val subscr1 = Subscription.make(sub.id, t1.id, "deliver", sub.created, sub.created)
        t1.subscriptionCount must equalTo(1)
        t2.subscriptionCount must equalTo(0)
        t3.subscriptionCount must equalTo(0)

        val subscr2 = Subscription.make(sub.id, t2.id, "deliver", sub.created, sub.created)
        val subscr3 = Subscription.make(sub2.id, t2.id, "deliver", sub2.created, sub2.created)
        t1.subscriptionCount must equalTo(1)
        t2.subscriptionCount must equalTo(2)
        t3.subscriptionCount must equalTo(0)
      }
    }

    "#subscriptions" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(u.id, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))


        Topic.all must haveSize(0)
        val s = Scheme.make("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s2 = Scheme.make("schemetag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t1 = Topic.make(s.id, "topictag1", "name")
        val t2 = Topic.make(s.id, "topictag2", "name2")
        val t3 = Topic.make(s2.id, "topictag3", "name3")

        t1.subscriptions.size must equalTo(0)
        t2.subscriptions.size must equalTo(0)
        t3.subscriptions.size must equalTo(0)

        // add subs to t1, count increases but does not affect t2 or t3
        val subscr1 = Subscription.make(sub.id, t1.id, "deliver", sub.created, sub.created)
        t1.subscriptions.size must equalTo(1)
        t2.subscriptions.size must equalTo(0)
        t3.subscriptions.size must equalTo(0)

        val subscr2 = Subscription.make(sub.id, t2.id, "deliver", sub.created, sub.created)
        val subscr3 = Subscription.make(sub2.id, t2.id, "deliver", sub2.created, sub2.created)
        val t1_subs = t1.subscriptions
        t1_subs.size must equalTo(1)
        t1_subs.contains(subscr1) must equalTo(true)
        t1_subs.contains(subscr2) must equalTo(false)
        t1_subs.contains(subscr3) must equalTo(false)

        val t2_subs = t2.subscriptions
        t2_subs.size must equalTo(2)
        t2_subs.contains(subscr1) must equalTo(false)
        t2_subs.contains(subscr2) must equalTo(true)
        t2_subs.contains(subscr3) must equalTo(true)

        t3.subscriptions.size must equalTo(0)
      }
    }
  }
}
