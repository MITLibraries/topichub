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

class ItemSpec extends Specification {

  "Item model" should {

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Item.create(1, 1, "loc", "scoap3:asdf:123")


        val Some(item) = Item.findById(1)
        item.objKey must equalTo("scoap3:asdf:123")

        //val Some(unfoundItem) = Item.findById(123)
        // unfoundItem must be empty

      }
    }

    "#findByKey" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Item.create(1, 1, "loc", "scoap3:asdf:123")

        val Some(item) = Item.findByKey("scoap3:asdf:123")
        item.objKey must equalTo("scoap3:asdf:123")

        //val Some(unfoundItem) = Item.findByKey("popcorn")
        //unfoundItem must be empty
      }
    }

    "#all" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pass", "roley")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat",
                               "pubstatus", Some(""), Some(""))
        val col = Collection.make(p.id, ct.id, rm.id, "coll", "desc", "open")
        val i1 = Item.make(col.id, ct.id, "loc", "scoap3:asdf:123")
        val i2 = Item.make(col.id, ct.id, "loc", "scoap3:asdf:456")

        val item_list = Item.all
        item_list must haveSize(2)
        item_list.contains(i1) must equalTo(true)
        item_list.contains(i2) must equalTo(true)
      }
    }

    "#allMissingMetadata" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pass", "roley")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat",
                               "pubstatus", Some(""), Some(""))
        val col = Collection.make(p.id, ct.id, rm.id, "coll", "desc", "open")
        val i1 = Item.make(col.id, ct.id, "loc", "scoap3:asdf:123")
        val i2 = Item.make(col.id, ct.id, "loc", "scoap3:asdf:456")

        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(s.id, "tag", "abstract")
        val item_list1 = Item.allMissingMetadata
        item_list1 must haveSize(2)
        item_list1.contains(i1) must equalTo(true)
        item_list1.contains(i2) must equalTo(true)

        i1.addTopic(t)
        val item_list2 = Item.allMissingMetadata
        item_list2 must haveSize(1)
        item_list2.contains(i1) must equalTo(false)
        item_list2.contains(i2) must equalTo(true)

        i2.addTopic(t)
        val item_list3 = Item.allMissingMetadata
        item_list3 must haveSize(0)
        item_list3.contains(i1) must equalTo(false)
        item_list3.contains(i2) must equalTo(false)
      }
    }

    "#allWithCatalogErrors" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val u = User.make("bob", "bob@example.com", "pass", "roley")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        val p = Publisher.make(u.id, "pubtag", "pubname", "pubdesc", "pubcat",
                               "pubstatus", Some(""), Some(""))
        val col = Collection.make(p.id, ct.id, rm.id, "coll", "desc", "open")
        val i1 = Item.make(col.id, ct.id, "loc", "scoap3:asdf:123")
        val i2 = Item.make(col.id, ct.id, "loc", "scoap3:asdf:456")

        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(s.id, "tag", "abstract")
        val t_any = Topic.make(s.id, "any", "Item with some topics")
        val t_none = Topic.make(s.id, "none", "Item with no topics")
        val item_list1 = Item.allWithCatalogErrors

        // items without any topics are in the error list
        item_list1 must haveSize(2)
        item_list1.contains(i1) must equalTo(true)
        item_list1.contains(i2) must equalTo(true)

        // adding a 'normal' topic does not remove the item from the error list
        i1.addTopic(t)
        val item_list2 = Item.allWithCatalogErrors
        item_list2 must haveSize(2)
        item_list2.contains(i1) must equalTo(true)
        item_list2.contains(i2) must equalTo(true)

        // adding the special 'any' topic removes an item from the error list
        i1.addTopic(t_any)
        val item_list3 = Item.allWithCatalogErrors
        item_list3 must haveSize(1)
        item_list3.contains(i1) must equalTo(false)
        item_list3.contains(i2) must equalTo(true)

        // adding the special 'none' topic removes an item from the error list
        i2.addTopic(t_none)
        val item_list4 = Item.allWithCatalogErrors
        item_list4 must haveSize(0)
        item_list4.contains(i1) must equalTo(false)
        item_list4.contains(i2) must equalTo(false)
      }
    }

    "#inCollection" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Item.create(1, 1, "loc", "scoap3:asdf:123")
        Collection.create(1, 1, 1, "coll2", "desc2", "open2")
        Item.create(2, 1, "loc", "scoap3:asdf:1")
        Item.create(2, 1, "loc", "scoap3:asdf:2")
        Item.create(2, 1, "loc", "scoap3:asdf:3")
        Item.create(2, 1, "loc", "scoap3:asdf:4")
        Item.create(2, 1, "loc", "scoap3:asdf:5")
        Item.create(2, 1, "loc", "scoap3:asdf:6")
        Item.create(2, 1, "loc", "scoap3:asdf:7")
        Item.create(2, 1, "loc", "scoap3:asdf:8")
        Item.create(2, 1, "loc", "scoap3:asdf:9")
        Item.create(2, 1, "loc", "scoap3:asdf:10")
        Item.create(2, 1, "loc", "scoap3:asdf:11")

        val item_list = Item.inCollection(2, 0)
        item_list must haveSize(10)

        val item_list2 = Item.inCollection(2, 1)
        item_list2 must haveSize(1)
      }
    }

    "#collectionCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Item.create(1, 1, "loc", "scoap3:asdf:123")
        Collection.create(1, 1, 1, "coll2", "desc2", "open2")
        Item.create(2, 1, "loc", "scoap3:asdf:1")
        Item.create(2, 1, "loc", "scoap3:asdf:2")
        Item.create(2, 1, "loc", "scoap3:asdf:3")
        Item.create(2, 1, "loc", "scoap3:asdf:4")
        Item.create(2, 1, "loc", "scoap3:asdf:5")
        Item.create(2, 1, "loc", "scoap3:asdf:6")
        Item.create(2, 1, "loc", "scoap3:asdf:7")
        Item.create(2, 1, "loc", "scoap3:asdf:8")
        Item.create(2, 1, "loc", "scoap3:asdf:9")
        Item.create(2, 1, "loc", "scoap3:asdf:10")
        Item.create(2, 1, "loc", "scoap3:asdf:11")

        Item.collectionCount(1) must equalTo(1)
        Item.collectionCount(2) must equalTo(11)
      }
    }

    "#createdAfterCount" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val now = System.currentTimeMillis
        val oldDate = new Date(now - 100000)
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Item.createdAfterCount(oldDate) must equalTo(0)
        Item.create(1, 1, "loc", "scoap3:asdf:123")
        Item.createdAfterCount(oldDate) must equalTo(1)
      }
    }

    "#filename for items with doi" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Item.create(1, 1, "loc", "scoap3:asdf:123")

        val item = Item.findById(1)
        item.get.addMetadata("doi", "asdf/stuff.asdf")

        val filename_pdf = item.get.filename("http://bob.pdf")
        filename_pdf must equalTo("stuff.asdf.pdf")

        val filename_pdfa = item.get.filename("http://bob.pdfa")
        filename_pdfa must equalTo("stuff.asdf_pdfa.pdf")

        val filename_xml = item.get.filename("http://bob.xml")
        filename_xml must equalTo("stuff.asdf.xml")

        val filename_error = item.get.filename("http://bob.popcorn")
        filename_error must equalTo("filename_error")
      }
    }

    "#filename for items without doi" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Item.create(1, 1, "loc", "scoap3:asdf:123")

        val item = Item.findById(1)

        val filename_pdf = item.get.filename("http://bob.pdf")
        filename_pdf must equalTo("filename_error")

        val filename_pdfa = item.get.filename("http://bob.pdfa")
        filename_pdfa must equalTo("filename_error")

        val filename_xml = item.get.filename("http://bob.xml")
        filename_xml must equalTo("filename_error")

        val filename_error = item.get.filename("http://bob.popcorn")
        filename_error must equalTo("filename_error")
      }
    }

    "#hasTopic" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "abstract")

        val topic = Topic.findById(1).get
        val item = Item.make(1, 1, "loc", "scoap3:asdf:123")
        item.hasTopic(topic) must equalTo(false)

        item.addTopic(topic)
        item.hasTopic(topic) must equalTo(true)
      }
    }

    "#topics" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "abstract")
        Topic.create(1, "tag", "meta")

        val item = Item.make(1, 1, "loc", "scoap3:asdf:123")
        item.addTopic(Topic.findById(1).get)
        item.addTopic(Topic.findById(2).get)
        item.topics must haveSize(2)
      }
    }

    "#regularTopics" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "abstract")
        Topic.create(1, "tag", "meta")

        val item = Item.make(1, 1, "loc", "scoap3:asdf:123")
        item.addTopic(Topic.findById(1).get)
        item.addTopic(Topic.findById(2).get)
        item.regularTopics must haveSize(1)
      }
    }

    "#contentType" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "abstract")
        Topic.create(1, "tag", "meta")

        val item = Item.make(1, 1, "loc", "scoap3:asdf:123")
        item.contentType must equalTo(ContentType.findById(1).get)
      }
    }

    "#metadataValue" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")

        val item = Item.make(1, 1, "loc", "scoap3:asdf:123")
        item.addMetadata("title", "Popcorn is Good")
        item.metadataValue("title") must equalTo("Popcorn is Good")
      }
    }

    "#metadataValue doesn't exist" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")

        val item = Item.make(1, 1, "loc", "scoap3:asdf:123")
        item.addMetadata("title", "Popcorn is Good")
        item.metadataValue("popcorn") must equalTo("Unknown Value")
      }
    }

    "#metadataValues" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")

        val item = Item.make(1, 1, "loc", "scoap3:asdf:123")
        item.addMetadata("title", "Popcorn is Good")
        item.addMetadata("author", "Some Guy")
        item.addMetadata("author", "Another Guy")
        item.metadataValues("author") must haveSize(2)
        item.metadataValues("author")(0) must equalTo("Some Guy")
        item.metadataValues("author")(1) must equalTo("Another Guy")
      }
    }

    "#toMets" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")

        val item = Item.make(1, 1, "loc", "scoap3:asdf:123")
        item.addMetadata("title", "Popcorn is Good")
        item.addMetadata("author", "Some Guy")
        item.addMetadata("author", "Another Guy")

        val mets = item.toMets
        (mets \\ "metsHdr").size must equalTo(1)
        (mets \\ "dmdSec").size must equalTo(1)
        (mets \\ "fileSec").size must equalTo(1)
        (mets \\ "structMap").size must equalTo(1)
        (mets \\ "valueString").size must equalTo(3)

        item.addMetadata("abstract", "More stuff!!!")
        val mets_abstract = item.toMets
        (mets_abstract \\ "valueString").size must equalTo(4)

        item.addMetadata("doi", "asdf//popcorn.123.456")
        item.addMetadata("additional_author", "Anonymoys Coward")
        item.addMetadata("publisher", "Important Company, Inc.")
        item.addMetadata("copyright_uri", "http://example.com/copyright_uri")
        item.addMetadata("copyright_holder", "The Authors")
        item.addMetadata("accessUri", "http://example.com/a.pdf")
        item.addMetadata("accessUri", "http://example.com/a.pdf?pdfa")
        item.addMetadata("accessUri", "http://example.com/a.xml")
        val mets_moar = item.toMets
        (mets_moar \\ "valueString").size must equalTo(9)
        (mets_moar \\ "FLocat").size must equalTo(3)
        (mets_moar \\ "fptr").size must equalTo(3)
      }
    }

    "#delete" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "orcid")
        Item.create(1, 1, "loc", "scoap3:asdf:xyz")

        val item = Item.make(1, 1, "loc", "scoap3:asdf:123")
        item.addTopic(Topic.findById(1).get)
        item.addMetadata("abstract", "Stuff about stuff")
        Item.all must haveSize(2)
        // todo: confirm topic and metadata table cleanup happened too

        Item.delete(item.id)
        Item.all must haveSize(1)
      }
    }

    "#deleteBefore" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.create("bob", "bob@example.com", "pass", "roley")
        ContentType.create("tag", "label", "desc", Some("logo"))
        ResourceMap.create("tag", "desc", Some("swordurl"))
        Publisher.create(1, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        Collection.create(1, 1, 1, "coll", "desc", "open")
        Scheme.create("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Topic.create(1, "tag", "orcid")

        val itemOlder = Item.make(1, 1, "loc", "scoap3:asdf:xyz")
        val item = Item.make(1, 1, "loc", "scoap3:asdf:123")
        Thread.sleep(500)
        val itemNewer = Item.make(1, 1, "loc", "scoap3:asdf:789")
        Item.all must haveSize(3)

        Item.deleteBefore(item.created)
        Item.all must haveSize(1)
      }
    }
  }
}
