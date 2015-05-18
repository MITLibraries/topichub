import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import models.Channel
import models.Subscriber
import models.User

class ChannelSpec extends Specification {

  "Channel model" should {

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        Channel.findById(1) must equalTo(None)
        Channel.create(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        Channel.findById(1).get.protocol must equalTo("protocol")
      }
    }

    "#findByUrl" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        Channel.findByUrl("http://example.com") must equalTo(None)
        Channel.create(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        Channel.findByUrl("http://example.com").get.protocol must equalTo("protocol")
      }
    }

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        Channel.findById(1) must equalTo(None)
        Channel.create(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        Channel.findById(1).get.protocol must equalTo("protocol")
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        Channel.findById(1) must equalTo(None)
        val c = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        c.protocol must equalTo("protocol")
      }
    }

    "#delete" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        Channel.findById(1) must equalTo(None)
        val c = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        Channel.findById(1) must not equalTo(None)
        Channel.delete(c.id)
        Channel.findById(1) must equalTo(None)
      }
    }

    "#recordTransfer" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        User.create("bob", "bob@example.com", "pwd", "role1")
        Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

        Channel.findById(1) must equalTo(None)
        var c = Channel.make(Subscriber.findById(1).get.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        c.transfers must equalTo(0)
        c.recordTransfer

        //need to reload to get new value
        c = Channel.findById(1).get
        c.transfers must equalTo(1)
      }
    }

    "#subscriber" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.all must haveSize(0)
        val user = User.make("bob", "bob@example.com", "pwd", "role1")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        var c = Channel.make(sub.id, "protocol", "mode", "description", "userid", "password", "http://example.com")
        c.subscriber must equalTo(sub)
      }
    }
  }
}
