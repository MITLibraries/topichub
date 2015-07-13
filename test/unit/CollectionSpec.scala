import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import models.{Collection, ContentType, Cull, Item, Publisher, ResourceMap, Scheme, Topic, User}

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

    "#findByTag only returns active Collections by default" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pass", "roley")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        val pub1 = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val pub2 = Publisher.make(u.id, "pubtag2", "pubname2", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        var c1 = Collection.make(pub1.id, ct.id, rm.id, "coll1", "desc", "open")
        var c2 = Collection.make(pub2.id, ct.id, rm.id, "coll2", "desc", "open", false)

        Collection.findByTag("coll1").contains(c1) must equalTo(true)
        Collection.findByTag("coll2").contains(c2) must equalTo(false)
      }
    }

    "#findByTag returns only inactive Collections if requested" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pass", "roley")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        val pub1 = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val pub2 = Publisher.make(u.id, "pubtag2", "pubname2", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        var c1 = Collection.make(pub1.id, ct.id, rm.id, "coll1", "desc", "open", false)

        Collection.findByTag("coll1").contains(c1) must equalTo(false)
        Collection.findByTag("coll1", false).contains(c1) must equalTo(true)
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

    "#publisher" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(1, "pubtag2", "pubname2", "pubdesc2", "pubcat2", "pubstatus2", Some(""), Some(""))

        Collection.findByPublisher(1).size must equalTo(0)
        Collection.findByPublisher(2).size must equalTo(0)

        val c1 = Collection.make(1, 1, 1, "coll1", "desc", "open")
        val c2 = Collection.make(2, 1, 1, "coll2", "desc", "open")

        c1.publisher must equalTo(Publisher.findById(1).get)
        c2.publisher must equalTo(Publisher.findById(2).get)
      }
    }

  }
}
