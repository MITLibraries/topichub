import org.specs2.mutable._
import org.specs2.runner._
import java.util.Date

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Channel, Collection, ContentType, Harvest, Item, Publisher, ResourceMap, Scheme,
                Subscriber, Topic, User }
import workers.Indexer

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class SearchPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")
  def item_factory(count: Int) {
    val ct = ContentType.make("tag", "label", "desc", Some("logo"))
    val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
    val user = User.make("pubuser", "pubuser@example.com", "", "pub_identity")
    val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                             Some(""), Some(""))
    val col = Collection.make(pub.id, ct.id, rm.id, "coll1_tag", "coll1 desc", "open")
  }

  "Search pages" should {
    "index" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/search")
      browser.pageSource must contain("Search for stuff!")
    }

    "can search for items by scheme" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      item_factory(1)
      val i = Item.make(1, 1, "location", "abc:123")
      val s1 = Scheme.make("scheme_1_tag", "gentype", "cat", "scheme_1 description",
                           Some("link"), Some("logo"))
      val s2 = Scheme.make("scheme_2_tag", "gentype", "cat", "scheme_2 description",
                           Some("link"), Some("logo"))
      val s3 = Scheme.make("scheme_with_no_items", "gentype", "cat", "scheme_2 description",
                          Some("link"), Some("logo"))
      i.addMetadata("title", "I like popcorn")
      val t1 = Topic.make(s1.id, "tag1", "name1")
      val t2 = Topic.make(s2.id, "tag2", "name2")
      i.addTopic(t1)
      i.addTopic(t2)
      Indexer.deindex_all("topic")
      Thread sleep 500
      Indexer.reindex("item")
      Thread sleep 500

      browser.goTo("http://localhost:" + port + "/search")
      browser.pageSource must contain("Search for stuff!")
      browser.$("#target_item").click
      browser.$("#inputQuery").text("scheme_1_tag")
      browser.$("#search_submit").click
      browser.pageSource must contain("Viewing all 1 records.")
      browser.pageSource must contain("I like popcorn")

      browser.goTo("http://localhost:" + port + "/search")
      browser.$("#target_item").click
      browser.$("#inputQuery").text("scheme_2_tag")
      browser.$("#search_submit").click
      browser.pageSource must contain("Viewing all 1 records.")
      browser.pageSource must contain("I like popcorn")

      browser.goTo("http://localhost:" + port + "/search")
      browser.$("#target_item").click
      browser.$("#inputQuery").text("scheme_with_no_items")
      browser.$("#search_submit").click
      browser.pageSource must contain("Viewing all 0 records.")
      browser.pageSource must not contain("I like popcorn")

      browser.goTo("http://localhost:" + port + "/search")
      browser.$("#target_item").click
      browser.$("#inputQuery").text("non_existing_scheme")
      browser.$("#search_submit").click
      browser.pageSource must contain("Viewing all 0 records.")
      browser.pageSource must not contain("I like popcorn")
      Indexer.deindex_all("item")
      Thread sleep 500
    }

    "can search for topics" in new WithBrowser(
      app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val s1 = Scheme.make("scheme_1_tag", "gentype", "cat", "scheme_1 description",
                           Some("link"), Some("logo"))
      val s2 = Scheme.make("scheme_2_tag", "gentype", "cat", "scheme_2 description",
                           Some("link"), Some("logo"))
      val s3 = Scheme.make("scheme_with_no_items", "gentype", "cat", "scheme_2 description",
                          Some("link"), Some("logo"))
      val t1 = Topic.make(s1.id, "Some Institution, Anytown, ST 02140", "No Label")
      val t2 = Topic.make(s2.id, "Dept. of Stuff, Some Institution, Anytown, ST 02140", "No Label")
      val t3 = Topic.make(s2.id, "Another Place, Anytown, ST 02140", "No Label")

      Indexer.deindex_all("item")
      Thread sleep 500
      Indexer.reindex("topic")
      Thread sleep 500

      browser.goTo("http://localhost:" + port + "/search")
      browser.pageSource must contain("Search for stuff!")
      browser.$("#inputQuery").text("Some Institution")
      browser.$("#search_submit").click
      browser.pageSource must contain("Viewing all 2 records.")
      browser.pageSource must contain(t1.tag)
      browser.pageSource must contain(t2.tag)
      browser.pageSource must not contain(t3.tag)

      Indexer.deindex_all("topic")
      Thread sleep 500
    }
  }
}
