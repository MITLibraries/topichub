import org.specs2.mutable._
import java.util.Date

import play.api.test._
import play.api.test.Helpers._

import models.{Scheme, Subscriber}
import models.{Agent, Topic, TopicPick, User}

class AgentSpec extends Specification {

  "Agent model" should {

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Agent.all must haveSize(0)
        Agent.create("tag", "label", "description", "code", "params", Some("icon"))
        Agent.create("tag2", "label", "description", "code", "params", Some("icon"))
        Agent.all must haveSize(2)
        Agent.all.contains(Agent.findById(1).get) must equalTo(true)
        Agent.all.contains(Agent.findById(2).get) must equalTo(true)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Agent.all must haveSize(0)
        Agent.create("tag", "label", "description", "code", "params", Some("icon"))
        Agent.create("tag2", "label", "description", "code", "params", Some("icon"))
        Agent.findById(1).get.tag must equalTo("tag")
      }
    }

    "#findByTag" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Agent.all must haveSize(0)
        Agent.create("tag", "label", "description", "code", "params", Some("icon"))
        Agent.create("tag2", "label", "description", "code", "params", Some("icon"))
        Agent.findByTag("tag").get.id must equalTo(1)
      }
    }

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Agent.all must haveSize(0)
        Agent.create("tag", "label", "description", "code", "params", Some("icon"))
        Agent.all must haveSize(1)
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Agent.all must haveSize(0)
        val a = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        Agent.all must haveSize(1)
        a.tag must equalTo("tag")
      }
    }

    "#mapView" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Agent.all must haveSize(0)
        Agent.create("tag", "label", "description", "code", "params", Some("icon"))
        Agent.create("tag2", "label", "description", "code", "params", Some("icon"))
        Agent.create("tag3", "label", "description", "code", "params", Some("icon"))
        Agent.mapView must havePair("1" -> "tag")
        Agent.mapView must havePair("2" -> "tag2")
        Agent.mapView must havePair("3" -> "tag3")
      }
    }
  }
}
