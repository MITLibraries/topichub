import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import models.Scheme
import models.Topic
import models.Validator
import java.util.Date

class TopicSpec extends Specification {

  "Topic model" should {

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "name")
        Topic.all must haveSize(1)
        Topic.findById(1).get.tag must equalTo("tag")
      }
    }

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "name")
        Topic.create(1, "tag2", "name2")
        Topic.all must haveSize(2)
        Topic.all.contains(Topic.findById(1).get) must equalTo(true)
        Topic.all.contains(Topic.findById(2).get) must equalTo(true)
      }
    }

    "#withScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag01", "name")
        Topic.create(1, "tag02", "name2")
        Topic.create(2, "other1", "name3")
        Topic.all must haveSize(3)
        Topic.withScheme(1, 0) must haveSize(2)
        Topic.withScheme(1, 0).contains(Topic.findById(1).get) must equalTo(true)
        Topic.withScheme(1, 0).contains(Topic.findById(2).get) must equalTo(true)
        Topic.withScheme(1, 0).contains(Topic.findById(3).get) must equalTo(false)
        Topic.withScheme(2, 0).contains(Topic.findById(3).get) must equalTo(true)

        Topic.create(1, "tag03", "name2")
        Topic.create(1, "tag04", "name2")
        Topic.create(1, "tag05", "name2")
        Topic.create(1, "tag06", "name2")
        Topic.create(1, "tag07", "name2")
        Topic.create(1, "tag08", "name2")
        Topic.create(1, "tag09", "name2")
        Topic.create(1, "tag10", "name2")
        Topic.create(1, "tag11", "name2")

        Topic.withScheme(1, 0) must haveSize(10)
        Topic.withScheme(1, 0).contains(Topic.findById(1).get) must equalTo(true)
        Topic.withScheme(1, 0).contains(Topic.findById(3).get) must equalTo(false)
        Topic.withScheme(1, 0).contains(Topic.findById(11).get) must equalTo(true)
        Topic.withScheme(1, 0).contains(Topic.findById(12).get) must equalTo(false)

        Topic.withScheme(1, 1) must haveSize(1)
        Topic.withScheme(1, 1).contains(Topic.findById(12).get) must equalTo(true)

        Topic.withScheme(1, 2) must haveSize(0)
      }
    }

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "name")
        Topic.all must haveSize(1)
      }
    }

    "#forSchemeAndTag" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Topic.all must haveSize(0)
        Scheme.create("schemetag1", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "topictag1", "name")
        Topic.forSchemeAndTag("schemetag1", "topictag1") must equalTo(Topic.findById(1))
        Topic.forSchemeAndTag("fakescheme", "topictag1") must equalTo(None)
        Topic.forSchemeAndTag("fakescheme", "faketopic") must equalTo(None)
        Topic.forSchemeAndTag("schemetag1", "faketopic") must equalTo(None)

        Topic.create(1, "topictag2", "name")
        Topic.forSchemeAndTag("schemetag1", "topictag1") must equalTo(Topic.findById(1))
        Topic.forSchemeAndTag("schemetag1", "topictag2") must equalTo(Topic.findById(2))
      }
    }

    "#recentItems" in {
      skipped
    }

    "#pagedItems" in {
      skipped
    }

    "#itemCount" in {
      skipped
    }

    "#itemCountSince" in {
      skipped
    }

    "#scheme" in {
      skipped
    }

    "#subscriptionCount" in {
      skipped
    }

    "#subscriptions" in {
      skipped
    }
  }
}
