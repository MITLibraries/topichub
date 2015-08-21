import org.specs2.mutable._
import org.specs2.runner._
import java.util.Date

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Collection, ContentType, Item, Publisher, ResourceMap, Scheme, Topic, User }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class OaiPmhSpec extends Specification {

  def item_factory(count: Int) {
    val ct = ContentType.make("tag", "label", "desc", Some("logo"))
    val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
    val user = User.make("pubuser", "pubuser@example.com", "", "pub_identity")
    val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                             Some(""), Some(""))
    val col = Collection.make(pub.id, ct.id, rm.id, "coll1_tag", "coll1 desc", "open")
    val col2 = Collection.make(pub.id, ct.id, rm.id, "coll2_tag", "coll2 desc", "open")
    1 to count foreach { n => Item.make(col.id, ct.id, "location", "abc:" + n) }
    1 to count foreach { n => Item.make(col2.id, ct.id, "location", "def:" + n) }
  }

  "OAIPMH pages" should {
    // GET /oai?verb=asdfasdf
    "Illegal Verb" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/oai?verb=asdfasdf")
      browser.pageSource must contain("Unknown verb: asdfasdf")
    }

    // GET /oai?verb=ListSets
    "ListSets" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val s1 = Scheme.make("Scheme1", "topic", "cat", "SomeScheme", Some("link"), Some("logo"))
      val t1 = Topic.make(s1.id, "Topic1", "Topic1 Name")
      val s2 = Scheme.make("Scheme2", "topic", "cat", "SomeScheme", Some("link"), Some("logo"))
      val t2 = Topic.make(s2.id, "Topic2", "Topic2 Name")
      val t3 = Topic.make(s2.id, "Topic3", "Topic3 Name")
      browser.goTo("http://localhost:" + port + "/oai?verb=ListSets")
      browser.pageSource must contain("Scheme1:Topic1")
      browser.pageSource must contain("Scheme2:Topic2")
      browser.pageSource must contain("Scheme2:Topic3")
    }

    // GET /oai?verb=ListRecords
    "ListRecords" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      item_factory(5)
      browser.goTo("http://localhost:" + port + "/oai?verb=ListRecords")
      browser.pageSource must contain("/item/2")
      browser.pageSource must contain("/item/package/2")
      browser.pageSource must contain("/item/10")
      browser.pageSource must contain("/item/package/10")
    }

    // GET /oai?verb=ListMetadataFormats
    "ListMetadataFormats" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/oai?verb=ListMetadataFormats")
      browser.pageSource must contain("http://www.openarchives.org/OAI/2.0/oai_dc.xsd")
      browser.pageSource must contain("http://www.openarchives.org/OAI/2.0/oai_dc/")
    }

    // GET /oai?verb=ListIdentifiers
    "ListIdentifiers with no matches" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/oai?verb=ListIdentifiers")
      browser.pageSource must contain("""<error code="noRecordsMatch"/>""")
    }

    // GET /oai?verb=ListIdentifiers
    "ListIdentifiers with matches" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      item_factory(5)
      browser.goTo("http://localhost:" + port + "/oai?verb=ListIdentifiers")
      browser.pageSource must contain("abc:1")
      browser.pageSource must contain("def:4")
      browser.pageSource must contain("def:5")
    }

    // GET /oai?verb=ListIdentifiers
    "ListIdentifiers with more matches than recLimit" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      item_factory(51)
      browser.goTo("http://localhost:" + port + "/oai?verb=ListIdentifiers")
      browser.pageSource must contain("""<resumptionToken completeListSize="102">""")
    }

    // GET /oai?verb=ListIdentifiers
    "ListIdentifiers with resumptionToken" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      pending("This would require parsing xml which so far this test suite has been avoiding. Feel free to add the test!")
    }

    // GET /oai?verb=Identify
    "Identify" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/oai?verb=Identify")
      browser.pageSource must contain("SCOAP3 Topic Hub")
      browser.pageSource must contain("topichub-admin@mit.edu")
    }

    // GET /oai?verb=GetRecord
    "GetRecord with no Identifier" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/oai?verb=GetRecord")
      browser.pageSource must contain("""<error code="badArgument">""")
      browser.pageSource must contain("Missing identifier")
    }

    // GET /oai?verb=GetRecord
    "GetRecord with no metadataPrefix" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/oai?verb=GetRecord&identifier=asdf")
      browser.pageSource must contain("""<error code="badArgument">""")
      browser.pageSource must contain("Missing metadataPrefix")
    }

    // GET /oai?verb=GetRecord
    "GetRecord with metadataPrefix is not oai_dc" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/oai?verb=GetRecord&identifier=asdf&metadataPrefix=fdsa")
      browser.pageSource must contain("""<error code="cannotDisseminateFormat"/>""")
    }

    // GET /oai?verb=GetRecord
    "GetRecord identifier does not exist" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/oai?verb=GetRecord&identifier=asdf&metadataPrefix=oai_dc")
      browser.pageSource must contain("""<error code="idDoesNotExist">""")
      browser.pageSource must contain("Not found: asdf")
    }

    // GET /oai?verb=GetRecord
    "GetRecord" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      item_factory(5)
      val i1 = Item.findById(1).get
      i1.addMetadata("title", "Das Title")
      i1.addMetadata("author", "Some Person")
      i1.addMetadata("author", "Another People")
      browser.goTo("http://localhost:" + port + "/oai?verb=GetRecord&identifier=abc:1&metadataPrefix=oai_dc")
      browser.pageSource must contain("Das Title")
      browser.pageSource must contain("Some Person")
      browser.pageSource must contain("Another People")
      browser.pageSource must contain("/item/1")
      browser.pageSource must contain("/item/package/1")
    }
  }
}
