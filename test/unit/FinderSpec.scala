import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import models.Finder
import models.Scheme
import models.ContentFormat

class FinderSpec extends Specification {

  "Finder model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))

        Finder.findByScheme(1).size must equalTo(0)
        Finder.create(s.id, cf.id, "description", "card", "idkey", "idlabel", "author")
        Finder.findByScheme(1).size must equalTo(1)
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))

        Finder.findById(1) must equalTo(None)
        val f = Finder.make(s.id, cf.id, "description", "card", "idkey", "idlabel", "author")
        Finder.findById(f.id) must equalTo(Some(f))
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))

        Finder.findById(1).size must equalTo(0)
        val f = Finder.make(s.id, cf.id, "description", "card", "idkey", "idlabel", "author")
        Finder.findById(f.id) must equalTo(Some(f))
      }
    }

    "#findByScheme" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        val s2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))

        Finder.findByScheme(s.id).size must equalTo(0)
        val f1 = Finder.make(s.id, 1, "description", "card", "idkey", "idlabel", "author")
        val f2 = Finder.make(s.id, 1, "description", "card", "idkey", "idlabel", "author")
        val f3 = Finder.make(s2.id, 1, "description", "card", "idkey", "idlabel", "author")

        val finders = Finder.findByScheme(s.id)
        finders.size must equalTo(2)
        finders.contains(f1) must equalTo(true)
        finders.contains(f2) must equalTo(true)
        finders.contains(f3) must equalTo(false)
      }
    }

    "#forSchemeAndFormat" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s1 = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val s2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf1 = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        val cf2 = ContentFormat.make("tag2", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))

        Finder.findByScheme(1).size must equalTo(0)
        val f1 = Finder.make(s1.id, cf1.id, "description", "card", "idkey", "idlabel", "author")
        val f2 = Finder.make(s1.id, cf1.id, "description", "card", "idkey", "idlabel", "author")
        val f3 = Finder.make(s2.id, cf1.id, "description", "card", "idkey", "idlabel", "author")
        val f4 = Finder.make(s1.id, cf2.id, "description", "card", "idkey", "idlabel", "author")

        val finders = Finder.forSchemeAndFormat(s1.id, f1.id)
        finders.size must equalTo(2)
        finders.contains(f1) must equalTo(true)
        finders.contains(f2) must equalTo(true)
        finders.contains(f3) must equalTo(false)
        finders.contains(f4) must equalTo(false)
      }
    }

    "#delete" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        ContentFormat.create("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))

        Finder.findById(1).size must equalTo(0)
        Finder.create(1, 1, "description", "card", "idkey", "idlabel", "author")
        Finder.findById(1).size must equalTo(1)
        Finder.delete(1)
        Finder.findById(1).size must equalTo(0)
      }
    }

    "#format" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        ContentFormat.create("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))

        Finder.findById(1).size must equalTo(0)
        Finder.create(1, 1, "description", "card", "idkey", "idlabel", "author")
        Finder.findById(1).get.format must equalTo(ContentFormat.findById(1))
      }
    }
  }
}
