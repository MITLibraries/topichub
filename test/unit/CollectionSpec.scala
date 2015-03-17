import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import models.Item
import models.Collection
import models.Publisher
import models.User
import models.ContentType
import models.ResourceMap
import models.Scheme
import models.Topic
import java.util.Date

class CollectionSpec extends Specification {

  "Collection model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        Collection.findAll must haveSize(0)
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Collection.findAll must haveSize(1)
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        Collection.findAll must haveSize(0)
        var c = Collection.make(1, 1, 1, "coll", "desc", "open")
        Collection.findAll must haveSize(1)
        c must equalTo(Collection.findById(1).get)
      }
    }

    "#findByPublisher" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(1, "pubtag2", "pubname2", "pubdesc2", "pubcat2", "pubstatus2", Some(""), Some(""))

        var c1 = Collection.make(1, 1, 1, "coll1", "desc", "open")
        var c2 = Collection.make(1, 1, 1, "coll2", "desc", "open")
        var c3 = Collection.make(2, 1, 1, "coll3", "desc", "open")

        var cols = Collection.findByPublisher(1)
        cols.contains(c1) must equalTo(true)
        cols.contains(c2) must equalTo(true)
        cols.contains(c3) must equalTo(false)
      }
    }

    "#findByTag" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(1, "pubtag2", "pubname2", "pubdesc2", "pubcat2", "pubstatus2", Some(""), Some(""))

        var c1 = Collection.make(1, 1, 1, "coll1", "desc", "open")
        var c2 = Collection.make(1, 1, 1, "coll2", "desc", "open")
        var c3 = Collection.make(2, 1, 1, "coll3", "desc", "open")

        var cols = Collection.findByTag("coll1")
        cols.contains(c1) must equalTo(true)
        cols.contains(c2) must equalTo(false)
        cols.contains(c3) must equalTo(false)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(1, "pubtag2", "pubname2", "pubdesc2", "pubcat2", "pubstatus2", Some(""), Some(""))

        var c1 = Collection.make(1, 1, 1, "coll1", "desc", "open")
        var c2 = Collection.make(1, 1, 1, "coll2", "desc", "open")
        var c3 = Collection.make(2, 1, 1, "coll3", "desc", "open")

        var cols = Collection.findById(1)
        cols.contains(c1) must equalTo(true)
        cols.contains(c2) must equalTo(false)
        cols.contains(c3) must equalTo(false)
      }
    }

    "#findAll" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(1, "pubtag2", "pubname2", "pubdesc2", "pubcat2", "pubstatus2", Some(""), Some(""))

        var c1 = Collection.make(1, 1, 1, "coll1", "desc", "open")
        var c2 = Collection.make(1, 1, 1, "coll2", "desc", "open")
        var c3 = Collection.make(2, 1, 1, "coll3", "desc", "open")

        var cols = Collection.findAll
        cols must haveSize(3)
        cols.contains(c1) must equalTo(true)
        cols.contains(c2) must equalTo(true)
        cols.contains(c3) must equalTo(true)
      }
    }

    "#recordDeposit" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(1, "pubtag2", "pubname2", "pubdesc2", "pubcat2", "pubstatus2", Some(""), Some(""))

        var c1 = Collection.make(1, 1, 1, "coll1", "desc", "open")

        c1.deposits must equalTo(0)
        c1.recordDeposit
        // necessary to reload to check updated value
        Collection.findById(c1.id).get.deposits must equalTo(1)
      }
    }

  }
}
