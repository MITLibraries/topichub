import org.specs2.mutable._
import org.specs2.matcher._
import org.specs2.matcher.MatchResult

import play.api.test._
import play.api.test.Helpers._
import models.Interest
import models.Scheme
import models.Subscriber
import models.User
import java.util.Date

class InterestSpec extends Specification {

  "Interest model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Interest.findById(1) must equalTo(None)
        Interest.create(sub.id, scheme.id, "action")
        Interest.findById(1).get.action must equalTo("action")
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Interest.findById(1) must equalTo(None)
        val i = Interest.make(sub.id, scheme.id, "action")
        i must equalTo(Interest.findById(1).get)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        val i = Interest.make(sub.id, scheme.id, "action")
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
        val i = Interest.make(sub.id, scheme.id, "action")
        val i2 = Interest.make(sub.id, scheme.id, "action_packed")
        val i3 = Interest.make(sub2.id, scheme.id, "action")

        val ints = Interest.findBySubscriber(sub.id)
        ints.size must equalTo(2)
        ints.contains(i) must equalTo(true)
        ints.contains(i2) must equalTo(true)
        ints.contains(i3) must equalTo(false)
      }
    }

    "#withScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val sub2 = Subscriber.make(1, "Sub2 Name2", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val scheme2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))


        Interest.withScheme(scheme.id, 0).size must equalTo(0)
        val i = Interest.make(sub.id, scheme.id, "action")
        val i2 = Interest.make(sub.id, scheme2.id, "action_packed")
        val i3 = Interest.make(sub2.id, scheme.id, "action")

        val ints = Interest.withScheme(scheme.id, 0)
        ints.size must equalTo(2)
        ints.contains(i) must equalTo(true)
        ints.contains(i2) must equalTo(false)
        ints.contains(i3) must equalTo(true)
      }
    }

    "#subscriber" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Interest.findById(1) must equalTo(None)
        val i = Interest.make(sub.id, scheme.id, "action")
        i.subscriber.get must equalTo(sub)
      }
    }

    "#scheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Interest.findById(1) must equalTo(None)
        val i = Interest.make(sub.id, scheme.id, "action")
        i.scheme.get must equalTo(scheme)
      }
    }
  }
}
