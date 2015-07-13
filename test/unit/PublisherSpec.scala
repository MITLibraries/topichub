import org.specs2.mutable._
import org.specs2.matcher._
import org.specs2.matcher.MatchResult
import java.util.Date
import play.api.test._
import play.api.test.Helpers._
import models.{Collection, ContentType, Cull, Harvest, Publisher, ResourceMap, User}

class PublisherSpec extends Specification {

  "Publisher model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        Publisher.all.size must equalTo(0)
        Publisher.create(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.all.size must equalTo(1)
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.all.size must equalTo(1)
        p.tag must equalTo("pubtag")
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(u.id, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.all.size must equalTo(2)
        Publisher.findById(p.id).get must equalTo(p)
      }
    }

    "#findByTag" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(u.id, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.all.size must equalTo(2)
        Publisher.findByTag(p.tag).get must equalTo(p)
      }
    }

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val u2 = User.make("bob2", "bob2@example.com", "pwd", "role1")

        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val p2 = Publisher.make(u.id, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val p3 = Publisher.make(u2.id, "pubtag3", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        val pubs = Publisher.all
        pubs.size must equalTo(3)
        pubs.contains(p) must equalTo(true)
        pubs.contains(p2) must equalTo(true)
        pubs.contains(p3) must equalTo(true)
      }
    }

    "#categories" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val u2 = User.make("bob2", "bob2@example.com", "pwd", "role1")

        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val p2 = Publisher.make(u.id, "pubtag2", "pubname", "pubdesc", "pubcat2", "pubstatus", Some(""), Some(""))
        val p3 = Publisher.make(u2.id, "pubtag3", "pubname", "pubdesc", "pubcat3", "pubstatus", Some(""), Some(""))
        val p4 = Publisher.make(u2.id, "pubtag4", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.all.size must equalTo(4)

        val pubcats = Publisher.categories
        pubcats.size must equalTo(3)
        pubcats.contains(p.category) must equalTo(true)
        pubcats.contains(p2.category) must equalTo(true)
        pubcats.contains(p3.category) must equalTo(true)
        pubcats.contains(p4.category) must equalTo(true)
      }
    }

    "#categoryCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val u2 = User.make("bob2", "bob2@example.com", "pwd", "role1")

        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val p2 = Publisher.make(u.id, "pubtag2", "pubname", "pubdesc", "pubcat2", "pubstatus", Some(""), Some(""))
        val p3 = Publisher.make(u2.id, "pubtag3", "pubname", "pubdesc", "pubcat3", "pubstatus", Some(""), Some(""))
        val p4 = Publisher.make(u2.id, "pubtag4", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.all.size must equalTo(4)

        Publisher.categoryCount(p.category) must equalTo(2)
        Publisher.categoryCount(p2.category) must equalTo(1)
        Publisher.categoryCount(p3.category) must equalTo(1)
        Publisher.categoryCount(p4.category) must equalTo(2)
      }
    }

    "#inCategory" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val u2 = User.make("bob2", "bob2@example.com", "pwd", "role1")

        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val p2 = Publisher.make(u.id, "pubtag2", "pubname", "pubdesc", "pubcat2", "pubstatus", Some(""), Some(""))
        val p3 = Publisher.make(u2.id, "pubtag3", "pubname", "pubdesc", "pubcat3", "pubstatus", Some(""), Some(""))
        val p4 = Publisher.make(u2.id, "pubtag4", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.all.size must equalTo(4)

        val pubcat = Publisher.inCategory(p.category, 0)
        pubcat.size must equalTo(2)
        pubcat.contains(p) must equalTo(true)
        pubcat.contains(p2) must equalTo(false)
        pubcat.contains(p3) must equalTo(false)
        pubcat.contains(p4) must equalTo(true)

        val pubcat_page2 = Publisher.inCategory(p.category, 1)
        pubcat_page2.size must equalTo(0)
      }
    }

    "#collectionCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))

        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        p.collectionCount must equalTo(0)
        Collection.create(p.id, ct.id, rm.id, "coll1", "desc", "open")
        Collection.create(p.id, ct.id, rm.id, "coll2", "desc", "open")
        Collection.create(p.id, ct.id, rm.id, "coll3", "desc", "open")
        p.collectionCount must equalTo(3)

        val p2 = Publisher.make(u.id, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(p2.id, ct.id, rm.id, "coll4", "desc", "open")
        p.collectionCount must equalTo(3)
      }
    }

    "#itemCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))

        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        p.itemCount must equalTo(0)
        val col = Collection.make(p.id, ct.id, rm.id, "coll1", "desc", "open")

        Publisher.findById(p.id).get.itemCount must equalTo(0)
        col.recordDeposit
        // must load new instance or incrementing doesn't work
        Collection.findById(col.id).get.recordDeposit
        Collection.findById(col.id).get.recordDeposit
        Publisher.findById(p.id).get.itemCount must equalTo(3)

        // a second collection for the same publisher increases the publisher item count
        val col2 = Collection.make(p.id, ct.id, rm.id, "coll2", "desc", "open")
        col2.recordDeposit
        Publisher.findById(p.id).get.itemCount must equalTo(4)

        // a collection for a different publisher does not increase the publisher item count
        val p2 = Publisher.make(u.id, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val col3 = Collection.make(p2.id, ct.id, rm.id, "coll3", "desc", "open")
        col3.recordDeposit
        Publisher.findById(p.id).get.itemCount must equalTo(4)
      }
    }

    "#harvests"  in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        p.harvests.size must equalTo(0)
        val h1 = Harvest.make(p.id, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        p.harvests.size must equalTo(1)
        p.harvests.contains(h1) must equalTo(true)
        val h2 = Harvest.make(p.id, "name2", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        p.harvests.contains(h1) must equalTo(true)
        p.harvests.contains(h2) must equalTo(true)
        p.harvests.size must equalTo(2)

        val p2 = Publisher.make(u.id, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val h3 = Harvest.make(p2.id, "name3", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        p.harvests.contains(h1) must equalTo(true)
        p.harvests.contains(h2) must equalTo(true)
        p.harvests.contains(h3) must equalTo(false)
        p.harvests.size must equalTo(2)
      }
    }

    "#harvestCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        // adding harvests for a publisher increases the publisher harvest count
        p.harvestCount must equalTo(0)
        Harvest.create(p.id, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        p.harvestCount must equalTo(1)
        Harvest.create(p.id, "name2", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        p.harvestCount must equalTo(2)

        // a harvest added to a different publisher does not increase the publisher harvest count
        val p2 = Publisher.make(u.id, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Harvest.create(p2.id, "name3", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        p.harvestCount must equalTo(2)
      }
    }

    "#cullCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pwd", "role1")
        Publisher.all.size must equalTo(0)
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        // adding harvests for a publisher increases the publisher harvest count
        p.cullCount must equalTo(0)
        Cull.create(p.id, "name", "soft", None, 1, new Date)
        p.cullCount must equalTo(1)
        Cull.create(p.id, "name2", "soft", None, 1, new Date)
        p.cullCount must equalTo(2)

        // a harvest added to a different publisher does not increase the publisher harvest count
        val p2 = Publisher.make(u.id, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Cull.create(p2.id, "name3", "soft", None, 1, new Date)
        p.cullCount must equalTo(2)
      }
    }
  }
}
