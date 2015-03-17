import org.specs2.mutable._
import org.specs2.matcher._
import org.specs2.matcher.MatchResult

import play.api.test._
import play.api.test.Helpers._
import models.ContentType
import models.Scheme
import java.util.Date

class ContentTypeSpec extends Specification {

  "ContentType model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentType.all must haveSize(0)
        ContentType.create("tag", "label", "desc", Some("logo"))
        ContentType.all must haveSize(1)
      }
    }

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentType.create("tag", "label", "desc", Some("logo"))
        ContentType.create("tag2", "label", "desc", Some("logo"))
        ContentType.create("tag3", "label", "desc", Some("logo"))
        val all = ContentType.all
        all must haveSize(3)
        all.contains(ContentType.findByTag("tag").get) must equalTo(true)
        all.contains(ContentType.findByTag("tag2").get) must equalTo(true)
        all.contains(ContentType.findByTag("tag3").get) must equalTo(true)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentType.create("tag", "label", "desc", Some("logo"))
        ContentType.create("tag2", "label", "desc", Some("logo"))
        ContentType.create("tag3", "label", "desc", Some("logo"))
        ContentType.all must haveSize(3)

        ContentType.findById(1) must equalTo(ContentType.findByTag("tag"))
        ContentType.findById(1) must not equalTo(ContentType.findByTag("tag2"))
      }
    }

    "#findByTag" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentType.create("tag", "label", "desc", Some("logo"))
        ContentType.create("tag2", "label", "desc", Some("logo"))
        ContentType.create("tag3", "label", "desc", Some("logo"))
        ContentType.all must haveSize(3)

        ContentType.findByTag("tag") must equalTo(ContentType.findById(1))
        ContentType.findByTag("tag") must not equalTo(ContentType.findById(2))
      }
    }

    "#mapView" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentType.create("tag", "label", "desc", Some("logo"))
        ContentType.create("tag2", "label", "desc", Some("logo"))
        ContentType.create("tag3", "label", "desc", Some("logo"))
        ContentType.all must haveSize(3)

        val map = ContentType.mapView
        map must havePair("1" -> "tag")
        map must havePair("2" -> "tag2")
        map must havePair("3" -> "tag3")
      }
    }

    "#schemes" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        ContentType.create("tag", "label", "desc", Some("logo"))
        val ct = ContentType.findById(1)
        ct.get.schemes("relation") must haveSize(0)
        ct.get.addScheme(Scheme.findById(1).get, "relation")
        ct.get.schemes("relation") must haveSize(1)
        ct.get.removeScheme(Scheme.findById(1).get, "relation")
        ct.get.schemes("relation") must haveSize(0)
      }
    }
  }
}
