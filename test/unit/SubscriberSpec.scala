import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._

import models.{Agent, Channel, Collection, ContentType}
import models.{Hold, Interest, Item, Plan, Publisher}
import models.ResourceMap
import models.Scheme
import models.Subscriber
import models.Subscription
import models.{Topic, TopicPick}
import models.Transfer
import models.User

import java.util.Date
import java.time.LocalDateTime
import java.time.YearMonth

class SubscriberSpec extends Specification {

  "Subscriber model" should {

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Subscriber.make(1, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))
        Subscriber.all must haveSize(2)
        Subscriber.findById(1).get.name must equalTo("Sub Name")
      }
    }

    "#findByUserId" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        User.create("bob2", "bob2@example.com", "pwd", "role1")
        Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Subscriber.make(2, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))
        Subscriber.all must haveSize(2)
        Subscriber.findByUserId(1).head.name must equalTo("Sub Name")
      }
    }

    "#findByUserId only finds approved users (not pending)" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        val u1 = User.make("bob", "bob@example.com", "pwd", "role1")
        val u2 = User.make("bob2", "bob2@example.com", "pwd", "role1")
        val s = Subscriber.make(u1.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        s.linkUser(u2.id)
        Subscriber.findByUserId(u1.id) must contain(s)
        Subscriber.findByUserId(u2.id) must not contain(s)

        s.approveUser(u2.id)
        Subscriber.findByUserId(u2.id) must contain(s)
      }
    }

    "#findByUserId handles multiple" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        val u1 = User.make("bob", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(u1.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val s2 = Subscriber.make(u1.id, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))
        Subscriber.findByUserId(u1.id) must contain(s)
        Subscriber.findByUserId(u1.id) must contain(s2)
      }
    }

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        User.create("bob2", "bob2@example.com", "pwd", "role1")
        Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Subscriber.make(2, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))
        Subscriber.all must haveSize(2)
        Subscriber.all.contains(Subscriber.findById(1).get) must equalTo(true)
        Subscriber.all.contains(Subscriber.findById(2).get) must equalTo(true)
      }
    }

    "#categories" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Subscriber.make(1, "Sub Name2", "cat2", "contact", Some("link"), Some("logo"))
        Subscriber.all must haveSize(2)
        Subscriber.categories.size must equalTo(2)
        Subscriber.categories.contains("cat") must equalTo(true)
        Subscriber.categories.contains("cat2") must equalTo(true)
        Subscriber.categories.contains("cat3") must equalTo(false)
      }
    }

    "#categoryCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Subscriber.make(1, "Sub Name2", "cat2", "contact", Some("link"), Some("logo"))
        Subscriber.make(1, "Sub Name3", "cat2", "contact", Some("link"), Some("logo"))
        Subscriber.all must haveSize(3)
        Subscriber.categories.size must equalTo(2)
        Subscriber.categoryCount("cat") must equalTo(1)
        Subscriber.categoryCount("cat2") must equalTo(2)
      }
    }

    "#inCategory" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        User.create("bob2", "bob2@example.com", "pwd", "role1")
        val s1 = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val s2 = Subscriber.make(2, "Sub Name2", "cat", "contact", Some("link"), Some("logo"))
        val s3 = Subscriber.make(2, "Sub Name2", "cat2", "contact", Some("link"), Some("logo"))

        Subscriber.all must haveSize(3)
        val cat = Subscriber.inCategory("cat", 0)
        cat.size must equalTo(2)
        cat.contains(s1) must equalTo(true)
        cat.contains(s2) must equalTo(true)
        cat.contains(s3) must equalTo(false)

        val cat2 = Subscriber.inCategory("cat2", 0)
        cat2.size must equalTo(1)
        cat2.contains(s1) must equalTo(false)
        cat2.contains(s2) must equalTo(false)
        cat2.contains(s3) must equalTo(true)
      }
    }

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Subscriber.create("Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Subscriber.all must haveSize(1)
        Subscriber.findById(1).get.name must equalTo("Sub Name")
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Subscriber.all must haveSize(1)
        s.name must equalTo("Sub Name")
      }
    }

    "#interests" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.interests must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        s.addInterest(Scheme.findById(1).get, "review")
        s.addInterest(Scheme.findById(2).get, "deliver")
        s.interests must haveSize(2)
      }
    }

    "#interestsCountIn" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(u.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.interests must haveSize(0)
        val sch1 = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val sch2 = Scheme.make("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        s.interestCountIn("tag") must equalTo(0)
        s.interestCountIn("tag1") must equalTo(0)
        s.addInterest(sch1, "MIT", true)
        s.addInterest(sch1, "Stanford", false)
        s.interestCountIn("tag") must equalTo(2)
        s.interestCountIn("tag1") must equalTo(0)
      }
    }

    "#interestIn" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.interests must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        s.addInterest(Scheme.findById(1).get, "review")
        s.interestIn(Scheme.findById(1).get.tag) must equalTo(Interest.findById(1))
      }
    }

    "#interestIn does not fail when Subscriber has no Interest in the Scheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val s2 = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.interests must haveSize(0)
        s2.interests must haveSize(0)

        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        //sub2 has an interest in a topic
        s2.addInterest(Scheme.findById(1).get, "review")
        s2.interestIn(Scheme.findById(1).get.tag) must equalTo(Interest.findById(1))

        //sub1 does not
        s.interestIn(Scheme.findById(1).get.tag) must equalTo(None)
      }
    }

    "#hasInterest" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.interests must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        s.addInterest(Scheme.findById(1).get, "review")
        s.hasInterest(Scheme.findById(1).get.tag) must equalTo(true)
      }
    }

    "#plans" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        sub.plans must haveSize(0)
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        val plan2 = Plan.make(sub.id, chan.id, "name2", "description", "thumbs-up", "review", "review", "subscribe", "review")
        sub.plans must haveSize(2)
      }
    }

    "#planFor" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        sub.plans must haveSize(0)
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        plan.addScheme(scheme)
        sub.planFor(scheme.id).get must equalTo(plan)
      }
    }

    "#plannedSchemes" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        sub.plans must haveSize(0)
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        val scheme1 = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        plan.addScheme(scheme1)
        val scheme2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        plan.addScheme(scheme2)
        sub.plannedSchemes.size must equalTo(2)
      }
    }

    "#unplannedSchemes" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        sub.plans must haveSize(0)
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        val scheme1 = Scheme.make("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        // scheme (topic-type) now exists, but not in plan - unplanned should be 1
        sub.unplannedSchemes.size must equalTo(1)
        plan.addScheme(scheme1)
        sub.unplannedSchemes.size must equalTo(0)
        // non-topic schemes should be ignored
        val scheme2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        sub.unplannedSchemes.size must equalTo(0)
        plan.addScheme(scheme2)
        sub.unplannedSchemes.size must equalTo(0)
      }
    }

    "#plannedFor" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        sub.plans must haveSize(0)
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        plan.addScheme(scheme)
        sub.plannedFor(Scheme.findById(1).get.id) must equalTo(true)
      }
    }

    "#subscriptionFor" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "name")
        s.subscriptionFor(Topic.findById(1).get.id) must equalTo(None)
        val subscr = Subscription.make(s.id, Topic.findById(1).get.id, "review", new Date(0), new Date(1000000000))
        s.subscriptionFor(Topic.findById(1).get.id) must equalTo(Some(subscr))
      }
    }

    "#subscribesTo(topicId)" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "name")
        s.subscribesTo(Topic.findById(1).get.id) must equalTo(false)
        val subscr = Subscription.make(s.id, Topic.findById(1).get.id, "review", new Date(0), new Date(1000000000))
        s.subscribesTo(Topic.findById(1).get.id) must equalTo(true)
      }
    }

    "#subscribeTo(topic)" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val plan = Plan.make(s.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        plan.addScheme(Scheme.findById(1).get)
        Topic.create(1, "tag", "name")
        s.addInterest(Scheme.findById(1).get, "review")
        s.subscribesTo(Topic.findById(1).get.id) must equalTo(false)
        s.subscribeTo(Topic.findById(1).get)
        s.subscribesTo(Topic.findById(1).get.id) must equalTo(true)
      }
    }

    "#templatesInScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.interests must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        s.addInterest(Scheme.findById(1).get, "MIT", true)
        s.addInterest(Scheme.findById(2).get, "Stanford")
        s.templatesInScheme("tag").size must equalTo(1)
        s.templatesInScheme("tag1").size must equalTo(0)
      }
    }

    "#interestWithValue" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.interests must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        s.addInterest(Scheme.findById(1).get, "MIT")
        s.addInterest(Scheme.findById(2).get, "Stanford", true)
        s.interestWithValue("tag", "MIT").size must equalTo(1)
        s.interestWithValue("tag1", "Stanford").size must equalTo(0)
      }
    }

    "#newInterestMapView" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Scheme.create("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.interests must haveSize(0)
        s.newInterestMapView must havePair("1" -> "tag")
        s.newInterestMapView must havePair("2" -> "tag2")
        s.newInterestMapView must not havePair("3" -> "tag3")

        s.addInterest(Scheme.findById(1).get, "review")
        s.newInterestMapView must not havePair("1" -> "tag")
        s.newInterestMapView must havePair("2" -> "tag2")
        s.newInterestMapView must not havePair("3" -> "tag3")
      }
    }

    "#addInterest" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Scheme.create("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.interests must haveSize(0)

        s.addInterest(Scheme.findById(1).get, "review")
        s.interests must haveSize(1)
        s.hasInterest(Scheme.findById(1).get.tag) must equalTo(true)
        s.hasInterest(Scheme.findById(2).get.tag) must equalTo(false)

        s.addInterest(Scheme.findById(2).get, "review")
        s.interests must haveSize(2)
        s.hasInterest(Scheme.findById(1).get.tag) must equalTo(true)
        s.hasInterest(Scheme.findById(2).get.tag) must equalTo(true)
      }
    }

    "#removeInterest" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Scheme.create("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.interests must haveSize(0)

        s.addInterest(Scheme.findById(1).get, "review")
        s.interests must haveSize(1)
        s.hasInterest(Scheme.findById(1).get.tag) must equalTo(true)
        s.hasInterest(Scheme.findById(2).get.tag) must equalTo(false)

        s.addInterest(Scheme.findById(2).get, "review")
        s.interests must haveSize(2)
        s.hasInterest(Scheme.findById(1).get.tag) must equalTo(true)
        s.hasInterest(Scheme.findById(2).get.tag) must equalTo(true)

        s.removeInterest(Scheme.findById(2).get, "review")
        s.interests must haveSize(1)
        s.hasInterest(Scheme.findById(1).get.tag) must equalTo(true)
        s.hasInterest(Scheme.findById(2).get.tag) must equalTo(false)
      }
    }

    "#holdCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Scheme.create("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.holdCount() must equalTo(0)
        Topic.create(1, "tag", "name")
        val subscr = Subscription.make(s.id, 1, "review", new Date(0), new Date(1000000000))
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        val i = Item.make(1, 1, "loc", "scoap3:asdf:123")
        val i2 = Item.make(1, 1, "loc", "scoap3:asdf:456")

        Hold.create(s.id, subscr.id, i.id)
        s.holdCount() must equalTo(1)

        Hold.create(s.id, subscr.id, i2.id)
        s.holdCount() must equalTo(2)
      }
    }

    "#holdCount can exclude and item" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Scheme.create("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.holdCount() must equalTo(0)
        Topic.create(1, "tag", "name")
        val subscr = Subscription.make(s.id, 1, "review", new Date(0), new Date(1000000000))
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        val i = Item.make(1, 1, "loc", "scoap3:asdf:123")
        val i2 = Item.make(1, 1, "loc", "scoap3:asdf:456")

        Hold.create(s.id, subscr.id, i.id)
        s.holdCount() must equalTo(1)

        Hold.create(s.id, subscr.id, i2.id)
        s.holdCount() must equalTo(2)
        s.holdCount(i2.id) must equalTo(1)
      }
    }

    "#holdOn" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Scheme.create("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.holdCount() must equalTo(0)
        Topic.create(1, "tag", "name")
        val subscr = Subscription.make(s.id, 1, "review", new Date(0), new Date(1000000000))
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        val i = Item.make(1, 1, "loc", "scoap3:asdf:123")
        val i2 = Item.make(1, 1, "loc", "scoap3:asdf:456")

        val h1 = Hold.make(s.id, subscr.id, i.id)
        s.holdCount() must equalTo(1)
        s.holdOn(i.id) must equalTo(Some(h1))
        s.holdOn(i2.id) must equalTo(None)

        val h2 = Hold.make(s.id, subscr.id, i2.id)
        s.holdCount() must equalTo(2)
        s.holdOn(i.id) must equalTo(Some(h1))
        s.holdOn(i2.id) must equalTo(Some(h2))
      }
    }

    "#holds" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Scheme.create("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.holdCount() must equalTo(0)
        Topic.create(1, "tag", "name")
        val subscr = Subscription.make(s.id, 1, "review", new Date(0), new Date(1000000000))
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        val i = Item.make(1, 1, "loc", "scoap3:asdf:123")
        val i2 = Item.make(1, 1, "loc", "scoap3:asdf:456")

        val h1 = Hold.make(s.id, subscr.id, i.id)
        val h2 = Hold.make(s.id, subscr.id, i2.id)
        s.holds(0).size must equalTo(2)
        s.holds(0)(0) must equalTo(h2)
        s.holds(0)(1) must equalTo(h1)

        val i3 = Item.make(1, 1, "loc", "scoap3:asdf:789")
        val i4 = Item.make(1, 1, "loc", "scoap3:asdf:1123")
        val i5 = Item.make(1, 1, "loc", "scoap3:asdf:1124")
        val i6 = Item.make(1, 1, "loc", "scoap3:asdf:1125")
        val i7 = Item.make(1, 1, "loc", "scoap3:asdf:1126")
        val i8 = Item.make(1, 1, "loc", "scoap3:asdf:1127")
        val i9 = Item.make(1, 1, "loc", "scoap3:asdf:1128")
        val i10 = Item.make(1, 1, "loc", "scoap3:asdf:1129")
        val i11 = Item.make(1, 1, "loc", "scoap3:asdf:1130")

        Hold.create(s.id, subscr.id, i3.id)
        Hold.create(s.id, subscr.id, i4.id)
        Hold.create(s.id, subscr.id, i5.id)
        Hold.create(s.id, subscr.id, i6.id)
        Hold.create(s.id, subscr.id, i7.id)
        Hold.create(s.id, subscr.id, i8.id)
        Hold.create(s.id, subscr.id, i9.id)
        Hold.create(s.id, subscr.id, i10.id)
        Hold.create(s.id, subscr.id, i11.id)

        s.holds(0, 0).size must equalTo(10)
        s.holds(1, 0).size must equalTo(1)
      }
    }

    "#holds can exclude an item" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Scheme.create("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.holdCount() must equalTo(0)
        Topic.create(1, "tag", "name")
        val subscr = Subscription.make(s.id, 1, "review", new Date(0), new Date(1000000000))
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        val i = Item.make(1, 1, "loc", "scoap3:asdf:123")
        val i2 = Item.make(1, 1, "loc", "scoap3:asdf:456")

        val h1 = Hold.make(s.id, subscr.id, i.id)
        val h2 = Hold.make(s.id, subscr.id, i2.id)

        s.holds(0, 0).size must equalTo(2)
        s.holds(0, i2.id).size must equalTo(1)
      }
    }

    "#pickCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Scheme.create("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.pickCount must equalTo(0)
        val top = Topic.make(1, "tag", "name")
        val top2 = Topic.make(1, "tag2", "name")
        val agent = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        TopicPick.create(s.id, top.id, agent.id, "imatch:1")
        s.pickCount must equalTo(1)
        TopicPick.create(s.id, top2.id, agent.id, "imatch:2")
        s.pickCount must equalTo(2)
      }
    }

    "#picked" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Scheme.create("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.holdCount() must equalTo(0)
        val top = Topic.make(1, "tag", "name")
        val top2 = Topic.make(1, "tag2", "name")
        val agent = Agent.make("tag", "label", "description", "code", "params", Some("icon"))

        val p1 = TopicPick.make(s.id, top.id, agent.id, "imatch:1")
        s.pickCount must equalTo(1)
        s.picked(top.id) must equalTo(Some(p1))
        s.picked(top2.id) must equalTo(None)

        val p2 = TopicPick.make(s.id, top2.id, agent.id, "imatch:2")
        s.pickCount must equalTo(2)
        s.picked(top.id) must equalTo(Some(p1))
        s.picked(top2.id) must equalTo(Some(p2))
      }
    }

    "#picks" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Scheme.create("tag", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "topic", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag3", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val agent = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        s.pickCount must equalTo(0)
        val t = Topic.make(1, "tag", "name")
        val t2 = Topic.make(1, "tag2", "name")

        val p1 = TopicPick.make(s.id, t.id, agent.id, "imatch:1")
        val p2 = TopicPick.make(s.id, t2.id, agent.id, "imatch:2")
        s.picks(0).size must equalTo(2)
        s.picks(0)(0) must equalTo(p2)
        s.picks(0)(1) must equalTo(p1)

        val t3 = Topic.make(1, "tag3", "name")
        val t4 = Topic.make(1, "tag4", "name")
        val t5 = Topic.make(1, "tag5", "name")
        val t6 = Topic.make(1, "tag6", "name")
        val t7 = Topic.make(1, "tag7", "name")
        val t8 = Topic.make(1, "tag8", "name")
        val t9 = Topic.make(1, "tag9", "name")
        val t10 = Topic.make(1, "tag10", "name")
        val t11 = Topic.make(1, "tag11", "name")

        TopicPick.create(s.id, t3.id, agent.id, "imatch:1")
        TopicPick.create(s.id, t4.id, agent.id, "imatch:2")
        TopicPick.create(s.id, t5.id, agent.id, "imatch:3")
        TopicPick.create(s.id, t6.id, agent.id, "imatch:4")
        TopicPick.create(s.id, t7.id, agent.id, "imatch:5")
        TopicPick.create(s.id, t8.id, agent.id, "imatch:6")
        TopicPick.create(s.id, t9.id, agent.id, "imatch:7")
        TopicPick.create(s.id, t10.id, agent.id, "imatch:8")
        TopicPick.create(s.id, t11.id, agent.id, "imatch:9")

        s.picks(0).size must equalTo(10)
        s.picks(1).size must equalTo(1)
      }
    }

    "#channels" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        s.channels must haveSize(0)

        val c = Channel.make(s.id, "sword", "mode", "desc", "userid", "password", "http://example.com")
        s.channels must haveSize(1)
        s.channels.contains(c) must equalTo(true)

        val c2 = Channel.make(s.id, "sword", "mode", "desc", "userid", "password", "http://example.com")
        s.channels must haveSize(2)
        s.channels.contains(c) must equalTo(true)
        s.channels.contains(c2) must equalTo(true)
      }
    }

    "#monthlyTransferSummary" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "name")
        val subscr = Subscription.make(s.id, 1, "review", new Date(0), new Date(1000000000))
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")

        val i = Item.make(1, 1, "loc", "scoap3:asdf:123")
        val start = (YearMonth now).minusMonths(11).atDay(1).atStartOfDay
        val end = (YearMonth now).atEndOfMonth.atTime(23, 59, 59)

        Transfer.create(s.id, subscr.id, i.id, "deliver")
        Transfer.create(s.id, subscr.id, i.id, "deliver")
        Transfer.create(s.id, subscr.id, i.id, "reject")

        val montTrans = s.monthlyTransferSummary(start, end)
        montTrans must haveSize(12)
        montTrans.last._1 must equalTo(end.getYear + "-" + end.getMonthValue)
        montTrans.last._2 must equalTo(List(3, 2, 0))
      }
    }

    "#transferCountByAction" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "name")
        val subscr = Subscription.make(s.id, 1, "review", new Date(0), new Date(1000000000))
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")

        val i = Item.make(1, 1, "loc", "scoap3:asdf:123")
        val start = LocalDateTime now
        val end = start.plusMonths(1)

        Transfer.create(s.id, subscr.id, i.id, "deliver")
        Transfer.create(s.id, subscr.id, i.id, "deliver")
        Transfer.create(s.id, subscr.id, i.id, "reject")
        s.transferCountByAction(start, end) must equalTo(List(3, 2, 0))
      }
    }

    "#userList" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        val user = User.make("bob", "bob@example.com", "pwd", "role1")
        val user2 = User.make("bob2", "bob2@example.com", "pwd", "role1")
        val s1 = Subscriber.make(user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        // make prelinks the user
        s1.userList().contains(user) must equalTo(true)
        s1.userList().contains(user2) must equalTo(false)
        s1.userList().size must equalTo(1)

        s1.linkUser(user2.id)
        s1.userList().contains(user) must equalTo(true)
        s1.userList().contains(user2) must equalTo(false)
        s1.userList(approved = false).contains(user2) must equalTo(true)
        s1.userList().size must equalTo(1)
      }
    }

    "#adminList" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("bob", "bob@example.com", "pwd", "role1")
        val user2 = User.make("bob2", "bob2@example.com", "pwd", "role1")
        val s1 = Subscriber.make(user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        // make prelinks the user
        s1.adminList.contains(user) must equalTo(true)
        s1.adminList.contains(user2) must equalTo(false)
        s1.adminList.size must equalTo(1)

        s1.linkUser(user2.id, approved = true)
        s1.adminList.contains(user) must equalTo(true)
        s1.adminList.contains(user2) must equalTo(false)
        s1.adminList.size must equalTo(1)
      }
    }

    "#makeAdmin" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("bob", "bob@example.com", "pwd", "role1")
        val user2 = User.make("bob2", "bob2@example.com", "pwd", "role1")
        val s1 = Subscriber.make(user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        // make prelinks the user
        s1.adminList.contains(user) must equalTo(true)
        s1.adminList.contains(user2) must equalTo(false)
        s1.adminList.size must equalTo(1)

        s1.linkUser(user2.id, approved = true)
        s1.adminList.contains(user) must equalTo(true)
        s1.adminList.contains(user2) must equalTo(false)
        s1.adminList.size must equalTo(1)

        s1.makeAdmin(user2.id)
        s1.adminList.contains(user) must equalTo(true)
        s1.adminList.contains(user2) must equalTo(true)
        s1.adminList.size must equalTo(2)
      }
    }

    "#removeAdmin" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("bob", "bob@example.com", "pwd", "role1")
        val user2 = User.make("bob2", "bob2@example.com", "pwd", "role1")
        val s1 = Subscriber.make(user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        // make prelinks the user
        s1.adminList.contains(user) must equalTo(true)
        s1.adminList.contains(user2) must equalTo(false)
        s1.adminList.size must equalTo(1)

        s1.linkUser(user2.id, approved = true, admin = true)
        s1.adminList.contains(user) must equalTo(true)
        s1.adminList.contains(user2) must equalTo(true)
        s1.adminList.size must equalTo(2)

        s1.removeAdmin(user2.id)
        s1.adminList.contains(user) must equalTo(true)
        s1.adminList.contains(user2) must equalTo(false)
        s1.adminList.size must equalTo(1)
      }
    }

    "#linkUser" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        val user = User.make("bob", "bob@example.com", "pwd", "role1")
        val user2 = User.make("bob2", "bob2@example.com", "pwd", "role1")
        val s1 = Subscriber.make(user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        // make prelinks the first user
        s1.userList().contains(user) must equalTo(true)
        s1.userList().contains(user2) must equalTo(false)
        s1.userList().size must equalTo(1)

        s1.linkUser(user2.id, approved = true)
        s1.userList().contains(user) must equalTo(true)
        s1.userList().contains(user2) must equalTo(true)
        s1.userList().size must equalTo(2)
      }
    }

    "#unlinkUser" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        val user = User.make("bob", "bob@example.com", "pwd", "role1")
        val user2 = User.make("bob2", "bob2@example.com", "pwd", "role1")
        val s1 = Subscriber.make(user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        // make prelinks the user
        s1.linkUser(user2.id, approved = true)
        s1.userList().contains(user) must equalTo(true)
        s1.userList().contains(user2) must equalTo(true)
        s1.userList().size must equalTo(2)

        s1.unlinkUser(user2.id)
        s1.userList().contains(user) must equalTo(true)
        s1.userList().contains(user2) must equalTo(false)
        s1.userList().size must equalTo(1)

        s1.unlinkUser(user.id)
        s1.userList().contains(user) must equalTo(false)
        s1.userList().contains(user2) must equalTo(false)
        s1.userList().size must equalTo(0)
      }
    }
  }
}
