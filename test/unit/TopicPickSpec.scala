import org.specs2.mutable._
import java.util.Date

import play.api.test._
import play.api.test.Helpers._

import models.{Scheme, Subscriber}
import models.{Agent, Topic, TopicPick, User}

class TopicPickSpec extends Specification {

  "TopicPick model" should {

    "#picked" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val agent = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        TopicPick.picked(top.id, s.id) must equalTo(false)
        val p = TopicPick.make(s.id, top.id, agent.id)
        TopicPick.picked(top.id, s.id) must equalTo(true)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val agent = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        TopicPick.picked(top.id, s.id) must equalTo(false)
        val p = TopicPick.make(s.id, top.id, agent.id)
        TopicPick.findById(1).get must equalTo(p)
      }
    }

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val agent = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        TopicPick.picked(top.id, s.id) must equalTo(false)
        TopicPick.create(s.id, top.id, agent.id)
        TopicPick.picked(top.id, s.id) must equalTo(true)
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val agent = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        TopicPick.picked(top.id, s.id) must equalTo(false)
        val p = TopicPick.make(s.id, top.id, agent.id)
        TopicPick.picked(top.id, s.id) must equalTo(true)
      }
    }

    "#topic" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val agent = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        val p = TopicPick.make(s.id, top.id, agent.id)
        p.topic must equalTo(top)
      }
    }

    "#resolve" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob1", "bob@example.com", "pwd", "role1")
        val s = Subscriber.make(1, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        Scheme.create("tag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val top = Topic.make(1, "tag", "name")
        val agent = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        val p = TopicPick.make(s.id, top.id, agent.id)
        TopicPick.picked(top.id, s.id) must equalTo(true)
        p.resolve(true)
        TopicPick.picked(top.id, s.id) must equalTo(false)

        val p2 = TopicPick.make(s.id, top.id, agent.id)
        TopicPick.picked(top.id, s.id) must equalTo(true)
        p2.resolve(false)
        TopicPick.picked(top.id, s.id) must equalTo(false)
      }
    }
  }
}
