import org.specs2.mutable._
import java.util.Date
import java.sql.Timestamp
import java.time.LocalDateTime
import java.time.Instant

import play.api.test._
import play.api.test.Helpers._
import models.Harvest
import models.Publisher
import models.User

class HarvestSpec extends Specification {

  "Harvest model" should {

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pass", "roley")
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Harvest.all must haveSize(0)

        val h = Harvest.make(p.id, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        Harvest.all must haveSize(1)
        Harvest.all must contain(h)

        val h2 = Harvest.make(p.id, "name2", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        Harvest.all must haveSize(2)
        Harvest.all must contain(h)
        Harvest.all must contain(h2)

        Harvest.delete(h2.id)
        Harvest.all must haveSize(1)
        Harvest.all must contain(h)
        Harvest.all must not contain(h2)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Harvest.findById(1) must equalTo(None)

        val h = Harvest.make(1, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        Harvest.findById(1).get must equalTo(h)
      }
    }

    "#findByPublisher" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(1, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        Harvest.findByPublisher(1).size must equalTo(0)
        val h1 = Harvest.make(1, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        val h2 = Harvest.make(1, "name2", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        val h3 = Harvest.make(2, "name3", "protocol", "http://www.example.com", "http://example.org", 1, new Date)

        val harvests = Harvest.findByPublisher(1)
        harvests.size must equalTo(2)
        harvests.contains(h1) must equalTo(true)
        harvests.contains(h2) must equalTo(true)
        harvests.contains(h3) must equalTo(false)
      }
    }

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Harvest.findById(1) must equalTo(None)

        Harvest.create(1, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        Harvest.findById(1) must not equalTo(None)
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        Harvest.findByPublisher(1).size must equalTo(0)
        val h1 = Harvest.make(1, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)

        h1.id must equalTo(1)
        h1.name must equalTo("name")
      }
    }

    "#delete" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Harvest.findById(1) must equalTo(None)

        val h = Harvest.make(1, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        Harvest.findById(1).get must equalTo(h)
        Harvest.delete(h.id)
        Harvest.findById(1) must equalTo(None)
      }
    }

    "#publisher" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(1, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        Harvest.findByPublisher(1).size must equalTo(0)
        Harvest.findByPublisher(2).size must equalTo(0)

        val h1 = Harvest.make(1, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        val h2 = Harvest.make(2, "name2", "protocol", "http://www.example.com", "http://example.org", 1, new Date)

        h1.publisher must equalTo(Publisher.findById(1))
        h2.publisher must equalTo(Publisher.findById(2))
      }
    }

    "#complete" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Harvest.findByPublisher(1).size must equalTo(0)

        val h1 = Harvest.make(1, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        val d1 = h1.updated
        h1.complete
        val d2 = Harvest.findById(h1.id).get.updated
        d1.toInstant.plusSeconds(h1.freq * 86400) must be equalTo(d2.toInstant)
      }
    }
  }
}
