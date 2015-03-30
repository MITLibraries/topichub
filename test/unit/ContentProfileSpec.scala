import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import models.ContentProfile
import models.Scheme

class ContentProfileSpec extends Specification {

  "ContentProfile model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentProfile.all.size must equalTo(0)
        ContentProfile.create("tag", "label", "description")
        ContentProfile.all.size must equalTo(1)
      }
    }

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentProfile.all.size must equalTo(0)
        ContentProfile.create("tag", "label", "description")
        ContentProfile.create("tag2", "label", "description")
        ContentProfile.all.size must equalTo(2)
        ContentProfile.all.contains(ContentProfile.findByTag("tag").get) must equalTo(true)
        ContentProfile.all.contains(ContentProfile.findByTag("tag2").get) must equalTo(true)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentProfile.all.size must equalTo(0)
        ContentProfile.create("tag", "label", "description")
        ContentProfile.create("tag2", "label", "description")
        ContentProfile.findById(1).get.tag must equalTo("tag")
      }
    }

    "#findByTag" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentProfile.all.size must equalTo(0)
        ContentProfile.create("tag", "label", "description")
        ContentProfile.create("tag2", "label", "description")
        ContentProfile.findByTag("tag").get.id must equalTo(1)
      }
    }

    "#mapView" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentProfile.all.size must equalTo(0)
        ContentProfile.create("tag", "label", "description")
        ContentProfile.create("tag2", "label", "description")
        val map = ContentProfile.mapView
        map.size must equalTo(2)
        map must havePair("1" -> "tag")
        map must havePair("2" -> "tag2")
      }
    }

    "#schemes" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentProfile.all.size must equalTo(0)
        ContentProfile.create("tag", "label", "description")
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        ContentProfile.findById(1).get.schemes.size must equalTo(0)
        ContentProfile.findById(1).get.addScheme(Scheme.findByTag("tag").get)
        ContentProfile.findById(1).get.addScheme(Scheme.findByTag("tag2").get)

        ContentProfile.findById(1).get.schemes.size must equalTo(2)
        ContentProfile.findById(1).get.schemes.contains(Scheme.findByTag("tag").get) must equalTo(true)
        ContentProfile.findById(1).get.schemes.contains(Scheme.findByTag("tag2").get) must equalTo(true)
      }
    }

    "#addScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentProfile.all.size must equalTo(0)
        ContentProfile.create("tag", "label", "description")
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        ContentProfile.findById(1).get.schemes.size must equalTo(0)

        ContentProfile.findById(1).get.addScheme(Scheme.findByTag("tag").get)
        ContentProfile.findById(1).get.schemes.size must equalTo(1)

        ContentProfile.findById(1).get.addScheme(Scheme.findByTag("tag2").get)
        ContentProfile.findById(1).get.schemes.size must equalTo(2)
      }
    }

    "#addScheme does not allow the same Scheme to be added multiple times" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentProfile.all.size must equalTo(0)
        ContentProfile.create("tag", "label", "description")
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        ContentProfile.findById(1).get.schemes.size must equalTo(0)

        ContentProfile.findById(1).get.addScheme(Scheme.findByTag("tag").get)
        ContentProfile.findById(1).get.schemes.size must equalTo(1)

        ContentProfile.findById(1).get.addScheme(Scheme.findByTag("tag").get)
        ContentProfile.findById(1).get.schemes.size must equalTo(1)
      }
    }

    "#removeScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ContentProfile.all.size must equalTo(0)
        ContentProfile.create("tag", "label", "description")
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Scheme.create("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        ContentProfile.findById(1).get.schemes.size must equalTo(0)

        ContentProfile.findById(1).get.addScheme(Scheme.findByTag("tag").get)
        ContentProfile.findById(1).get.addScheme(Scheme.findByTag("tag2").get)
        ContentProfile.findById(1).get.schemes.size must equalTo(2)

        ContentProfile.findById(1).get.removeScheme(Scheme.findByTag("tag").get)
        ContentProfile.findById(1).get.schemes.size must equalTo(1)
        ContentProfile.findById(1).get.schemes.contains(Scheme.findByTag("tag").get) must equalTo(false)
        ContentProfile.findById(1).get.schemes.contains(Scheme.findByTag("tag2").get) must equalTo(true)
      }
    }
  }
}
