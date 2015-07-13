import org.specs2.mutable._
import java.util.Date
import java.sql.Timestamp
import java.time.LocalDateTime
import java.time.Instant

import play.api.test._
import play.api.test.Helpers._
import models.{Cull, Publisher, User}

class CullSpec extends Specification {

  "Cull model" should {

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pass", "roley")
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Cull.all must haveSize(0)

        val c = Cull.make(p.id, "name", "soft", Option("mailto:bob@repo.example.com"), 1, new Date)
        Cull.all must haveSize(1)
        Cull.all must contain(c)

        val c2 = Cull.make(p.id, "name2", "soft", Option("mailto:bob@repo.example.com"), 1, new Date)
        Cull.all must haveSize(2)
        Cull.all must contain(c)
        Cull.all must contain(c2)

        Cull.delete(c2.id)
        Cull.all must haveSize(1)
        Cull.all must contain(c)
        Cull.all must not contain(c2)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Cull.findById(1) must equalTo(None)

        val c = Cull.make(1, "name", "soft", Option("mailto:bob@repo.example.com"), 1, new Date)
        Cull.findById(1).get must equalTo(c)
      }
    }

    "#findByPublisher" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(1, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        Cull.findByPublisher(1).size must equalTo(0)
        val c1 = Cull.make(1, "name", "soft", Option("mailto:bob@repo.example.com"), 1, new Date)
        val c2 = Cull.make(1, "name2", "soft", Option("mailto:bob@repo.example.com"), 1, new Date)
        val c3 = Cull.make(2, "name3", "soft", Option("mailto:bob@repo.example.com"), 1, new Date)

        val culls = Cull.findByPublisher(1)
        culls.size must equalTo(2)
        culls.contains(c1) must equalTo(true)
        culls.contains(c2) must equalTo(true)
        culls.contains(c3) must equalTo(false)
      }
    }

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Cull.findById(1) must equalTo(None)

        Cull.create(1, "name", "soft", Option("mailto:bob@repo.example.com"), 1, new Date)
        Cull.findById(1) must not equalTo(None)
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        Cull.findByPublisher(1).size must equalTo(0)
        val c1 = Cull.make(1, "name", "soft", None, 1, new Date)

        c1.id must equalTo(1)
        c1.name must equalTo("name")
      }
    }

    "#delete" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Cull.findById(1) must equalTo(None)

        val c = Cull.make(1, "name", "soft", None, 1, new Date)
        Cull.findById(1).get must equalTo(c)
        Cull.delete(c.id)
        Cull.findById(1) must equalTo(None)
      }
    }

    "#publisher" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Publisher.create(1, "pubtag2", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        Cull.findByPublisher(1).size must equalTo(0)
        Cull.findByPublisher(2).size must equalTo(0)

        val c1 = Cull.make(1, "name", "soft", None, 1, new Date)
        val c2 = Cull.make(2, "name2", "soft", None, 1, new Date)

        c1.publisher must equalTo(Publisher.findById(1))
        c2.publisher must equalTo(Publisher.findById(2))
      }
    }

    "#complete" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Cull.findByPublisher(1).size must equalTo(0)

        val c1 = Cull.make(1, "name", "soft", None, 1, new Date)
        val d1 = c1.updated
        c1.complete
        val d2 = Cull.findById(c1.id).get.updated
        d1.toInstant.plusSeconds(c1.freq * 86400) must be equalTo(d2.toInstant)
      }
    }
  }
}
