import org.specs2.mutable._
import org.specs2.matcher._
import org.specs2.matcher.MatchResult

import play.api.test._
import play.api.test.Helpers._
import models.{Channel, Interest, Plan, Scheme, Subscriber, Subscription, Topic, User}
import java.util.Date

class InterestSpec extends Specification {

  "Interest model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Interest.findById(1) must equalTo(None)
        Interest.create(sub.id, scheme.tag, "MIT", false)
        Interest.findById(1).get.intValue must equalTo("MIT")
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Interest.findById(1) must equalTo(None)
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        i must equalTo(Interest.findById(1).get)
      }
    }

    "#delete" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Interest.findById(1) must equalTo(None)
        Interest.create(sub.id, scheme.tag, "MIT", false)
        Interest.findById(1).get.intValue must equalTo("MIT")
        Interest.delete(1)
        Interest.findById(1) must equalTo(None)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        Interest.findById(1).get must equalTo(i)
      }
    }

    "#findBySubscriber" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(1, "Sub2 Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Interest.findBySubscriber(sub.id).size must equalTo(0)
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        val i2 = Interest.make(sub.id, scheme.tag, "Harvard", false)
        val i3 = Interest.make(sub2.id, scheme.tag, "MIT", false)

        val ints = Interest.findBySubscriber(sub.id)
        ints.size must equalTo(2)
        ints.contains(i) must equalTo(true)
        ints.contains(i2) must equalTo(true)
        ints.contains(i3) must equalTo(false)
      }
    }

    "#schemeCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(1, "Sub2 Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val scheme2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))


        Interest.schemeCount(sub.id, scheme.tag) must equalTo(0)
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        val i2 = Interest.make(sub.id, scheme.tag, "Harvard", false)
        val i3 = Interest.make(sub2.id, scheme.tag, "MIT", false)

        Interest.schemeCount(sub.id, scheme.tag) must equalTo(2)
      }
    }

    "#inScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(1, "Sub2 Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val scheme2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Interest.inScheme(sub.id, scheme.tag, 0).size must equalTo(0)
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        val i2 = Interest.make(sub.id, scheme.tag, "Harvard", false)
        val i3 = Interest.make(sub2.id, scheme.tag, "MIT", false)

        val ints = Interest.inScheme(sub.id, scheme.tag, 0)
        ints.size must equalTo(2)
        ints.contains(i) must equalTo(true)
        ints.contains(i2) must equalTo(true)
        ints.contains(i3) must equalTo(false)
      }
    }

    "#planCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(1, "Sub2 Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val scheme2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")

        plan.schemes.size must equalTo(0)
        Interest.planCount(sub.id, plan.id) must equalTo(0)
        plan.addScheme(scheme)

        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        val i2 = Interest.make(sub.id, scheme2.tag, "Harvard", false)
        val i3 = Interest.make(sub2.id, scheme.tag, "MIT", false)

        Interest.planCount(sub.id, plan.id) must equalTo(1)
      }
    }

    "#inPlan" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(1, "Sub2 Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val scheme2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")

        plan.schemes.size must equalTo(0)
        Interest.planCount(sub.id, plan.id) must equalTo(0)
        plan.addScheme(scheme)
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        val i2 = Interest.make(sub.id, scheme2.tag, "Harvard", false)
        val i3 = Interest.make(sub2.id, scheme.tag, "MIT", false)

        Interest.inPlan(sub.id, plan.id, 0).size must equalTo(1)
      }
    }

    "#matchCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(1, "Sub2 Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val topic = Topic.make(scheme.id, "tag", "name")
        val subscrip = Subscription.make(sub.id, topic.id, "deliver", sub.created, sub.created)
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)

        Interest.matchCount(sub.id, "sub") must equalTo(0)
        subscrip.linkInterest(i)
        Interest.matchCount(sub.id, "sub") must equalTo(1)
        Interest.matchCount(sub.id, "unsub") must equalTo(0)
      }
    }

    "#inMatch" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(1, "Sub2 Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val topic = Topic.make(scheme.id, "tag", "name")
        val subscrip = Subscription.make(sub.id, topic.id, "deliver", sub.created, sub.created)
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)

        Interest.inMatch(sub.id, "sub", 0).size must equalTo(0)
        subscrip.linkInterest(i)
        Interest.inMatch(sub.id, "sub", 0).size must equalTo(1)
        Interest.inMatch(sub.id, "unsub", 0).size must equalTo(0)
      }
    }

    "#unmatched" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(1, "Sub2 Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val topic = Topic.make(scheme.id, "tag", "name")
        val subscrip = Subscription.make(sub.id, topic.id, "deliver", sub.created, sub.created)
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)

        Interest.unmatched(scheme.tag).size must equalTo(1)
        subscrip.linkInterest(i)
        Interest.unmatched(scheme.tag).size must equalTo(0)
      }
    }

    "#templates" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(1, "Sub2 Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val topic = Topic.make(scheme.id, "tag", "name")
        val subscrip = Subscription.make(sub.id, topic.id, "deliver", sub.created, sub.created)
        val i = Interest.make(sub.id, scheme.tag, "MIT", true)

        Interest.templates(scheme.tag).size must equalTo(1)
        Interest.delete(i.id)
        Interest.templates(scheme.tag).size must equalTo(0)
      }
    }


    "#subscriber" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Interest.findById(1) must equalTo(None)
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        i.subscriber must equalTo(sub)
      }
    }

    "#scheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Interest.findById(1) must equalTo(None)
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        i.scheme must equalTo(scheme)
      }
    }
  }
}
