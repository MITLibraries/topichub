import org.specs2.mutable._
import org.specs2.matcher._
import org.specs2.matcher.MatchResult
import play.api.test._
import play.api.test.Helpers._
import models.ContentFormat
import models.ResourceMap
import models.Scheme

class ResourceMapSpec extends Specification {

  "ResourceMap model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        ResourceMap.create("tag", "desc", Some("http://www.example.com"))
        ResourceMap.all.size must equalTo(1)
      }
    }

    "#create does not require a sword url" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        ResourceMap.create("tag", "desc", None)
        ResourceMap.all.size must equalTo(1)
      }
    }.pendingUntilFixed("See https://github.com/MITLibraries/scoap3hub/issues/137")

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        val rm = ResourceMap.make("tag", "desc", Some("http://www.example.com"))
        ResourceMap.all.size must equalTo(1)
        rm.tag must equalTo("tag")
      }
    }

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        val rm = ResourceMap.make("tag", "desc", Some("http://www.example.com"))
        val rm2 = ResourceMap.make("tag2", "desc", Some("http://www.example.com"))
        val rm3 = ResourceMap.make("tag3", "desc", Some("http://www.example.com"))
        val rms = ResourceMap.all
        rms.size must equalTo(3)
        rms.contains(rm) must equalTo(true)
        rms.contains(rm2) must equalTo(true)
        rms.contains(rm3) must equalTo(true)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        val rm = ResourceMap.make("tag", "desc", Some("http://www.example.com"))
        val rm2 = ResourceMap.make("tag2", "desc", Some("http://www.example.com"))
        ResourceMap.findById(rm.id).get must equalTo(rm)
      }
    }

    "#findByTag" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        val rm = ResourceMap.make("tag", "desc", Some("http://www.example.com"))
        val rm2 = ResourceMap.make("tag2", "desc", Some("http://www.example.com"))
        ResourceMap.findByTag(rm.tag).get must equalTo(rm)
      }
    }

    "#mapView" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        val rm = ResourceMap.make("tag", "desc", Some("http://www.example.com"))
        val rm2 = ResourceMap.make("tag2", "desc", Some("http://www.example.com"))
        val rm3 = ResourceMap.make("tag3", "desc", Some("http://www.example.com"))
        val rmMap = ResourceMap.mapView
        rmMap.size must equalTo(3)
        rmMap must havePair("1" -> "tag")
        rmMap must havePair("2" -> "tag2")
        rmMap must havePair("3" -> "tag3")
      }
    }

    "#addMapping" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("http://www.example.com"))

        rm.mappingsForScheme(s).size must equalTo(0)
        rm.addMapping(s.id, cf.id, "source", 1)
        rm.mappingsForScheme(s).size must equalTo(1)
      }
    }

    "#addMapping does not allow the same mapping to be added multiple times" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("http://www.example.com"))

        rm.mappingsForScheme(s).size must equalTo(0)
        rm.addMapping(s.id, cf.id, "source", 1)
        rm.addMapping(s.id, cf.id, "source", 1) must throwA[Exception].like {
                case e: Exception => e.getMessage aka "error" mustEqual (
                  "JdbcSQLException(Unique index or primary key violation)")}
      }.pendingUntilFixed("https://github.com/MITLibraries/scoap3hub/issues/138")
    }

    "#mappingsForScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("http://www.example.com"))

        rm.mappingsForScheme(s).size must equalTo(0)
        rm.addMapping(s.id, cf.id, "source", 1)
        rm.addMapping(s.id, cf.id, "source2", 1)
        val rmMaps = rm.mappingsForScheme(s)
        rmMaps.size must equalTo(2)
        rmMaps.contains("source", s.id, cf.id) must equalTo(true)
        rmMaps.contains("source2", s.id, cf.id) must equalTo(true)
        rmMaps.contains("fake thing", s.id, cf.id) must equalTo(false)
      }
    }

    "#schemes" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))

        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("http://www.example.com"))
        rm.schemes.size must equalTo(0)

        rm.addMapping(s.id, cf.id, "source", 1)
        rm.addMapping(s.id, cf.id, "source2", 1)
        rm.addMapping(s2.id, cf.id, "source", 1)
        rm.schemes.size must equalTo(2)
      }
    }

    "#removeMapping" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        ResourceMap.all.size must equalTo(0)
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("http://www.example.com"))

        rm.mappingsForScheme(s).size must equalTo(0)
        rm.addMapping(s.id, cf.id, "source", 1)
        rm.mappingsForScheme(s).size must equalTo(1)
        rm.removeMapping(s, "source")
        rm.mappingsForScheme(s).size must equalTo(0)
      }
    }
  }
}
