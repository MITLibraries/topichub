import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import models.ContentFormat
import java.util.Date

class ContentFormatSpec extends Specification {

  "ContentFormat model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentFormat.all must haveSize(0)
        ContentFormat.create("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        ContentFormat.all must haveSize(1)
      }
    }

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentFormat.all must haveSize(0)
        ContentFormat.create("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        ContentFormat.create("tag2", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        val cf = ContentFormat.all
        cf must haveSize(2)
        cf.contains(ContentFormat.findById(1).get) must equalTo(true)
        cf.contains(ContentFormat.findById(2).get) must equalTo(true)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentFormat.all must haveSize(0)
        ContentFormat.create("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        ContentFormat.create("tag2", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        ContentFormat.all must haveSize(2)
        ContentFormat.findById(1).get.tag must equalTo("tag")
      }
    }

    "#findByTag" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentFormat.all must haveSize(0)
        ContentFormat.create("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        ContentFormat.create("tag2", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        ContentFormat.all must haveSize(2)
        ContentFormat.findByTag("tag").get.id must equalTo(1)
      }
    }

    "#mapView" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentFormat.all must haveSize(0)
        ContentFormat.create("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        ContentFormat.create("tag2", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        ContentFormat.all must haveSize(2)
        val map = ContentFormat.mapView
        map must havePair("1" -> "tag")
        map must havePair("2" -> "tag2")
      }
    }
  }
}
