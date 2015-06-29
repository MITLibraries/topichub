import org.specs2.mutable._
import org.specs2.matcher._
import org.specs2.matcher.MatchResult

import play.api.test._
import play.api.test.Helpers._
import models.{Plan, Channel, Scheme, Subscriber, User}

import java.util.Date

class PlanSpec extends Specification {

  "Plan model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")

        Plan.findById(1) must equalTo(None)
        Plan.create(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        Plan.findById(1).get.fulfill must equalTo("deliver")
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")

        Plan.findById(1) must equalTo(None)
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        p must equalTo(Plan.findById(1).get)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")

        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        Plan.findById(1).get must equalTo(p)
      }
    }

    "#findBySubscriber" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(1, "Sub2 Name2", "cat", "contact", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")

        Plan.findBySubscriber(sub.id).size must equalTo(0)
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        val p2 = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        val p3 = Plan.make(sub2.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")

        val plans = Plan.findBySubscriber(sub.id)
        plans.size must equalTo(2)
        plans.contains(p) must equalTo(true)
        plans.contains(p2) must equalTo(true)
        plans.contains(p3) must equalTo(false)
      }
    }

    "#delete" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")

        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        Plan.findById(1).get must equalTo(p)
        Plan.delete(p.id)
        Plan.findById(1) must equalTo(None)
      }
    }

    "#subscriber" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")

        Plan.findById(1) must equalTo(None)
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        p.subscriber.get must equalTo(sub)
      }
    }

    "#channel" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")

        Plan.findById(1) must equalTo(None)
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        p.channel.get must equalTo(chan)
      }
    }

    "#addScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Plan.findById(1) must equalTo(None)
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        p.schemes.size must equalTo(0)
        p.addScheme(scheme)
        p.schemes.head must equalTo(scheme)
      }
    }

    "#removeScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Plan.findById(1) must equalTo(None)
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        p.schemes.size must equalTo(0)
        p.addScheme(scheme)
        p.schemes.size must equalTo(1)
        p.removeScheme(scheme)
        p.schemes.size must equalTo(0)
      }
    }

    "#schemes" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val scheme2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Plan.findById(1) must equalTo(None)
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        p.schemes.size must equalTo(0)
        p.addScheme(scheme)
        p.addScheme(scheme2)
        p.schemes.size must equalTo(2)
      }
    }

    "#setChannel" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val chan = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        val chan2 = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")

        Plan.findById(1) must equalTo(None)
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver", "review", "subscribe", "review")
        p.channel.get.id must equalTo(chan.id)
        p.setChannel(chan2)
        Plan.findById(1).get.channel.get.id must equalTo(chan2.id)
      }
    }
  }
}
