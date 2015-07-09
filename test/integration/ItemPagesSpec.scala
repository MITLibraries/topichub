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

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class ItemPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")
  def item_factory(count: Int) {
    val ct = ContentType.make("tag", "label", "desc", Some("logo"))
    val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
    val user = User.make("pubuser", "pubuser@example.com", "", "pub_identity")
    val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                             Some(""), Some(""))
    val col = Collection.make(pub.id, ct.id, rm.id, "coll1_tag", "coll1 desc", "open")
    1 to count foreach { n => Item.make(col.id, ct.id, "location", "abc:" + n) }
  }

  "Item pages" should {
    "as an unauthenticated User" should {

      // GET /items/browse
      "browsing items works" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        item_factory(11)
        Item.all.size must equalTo(11)
        browser.goTo("http://localhost:" + port + "/items/browse?filter=collection&id=1")
        assertThat(browser.title()).isEqualTo("Item Browse - TopicHub")
        browser.pageSource must contain("Viewing 1 - 10 of 11")
        browser.$("#next_page").click
        browser.pageSource must contain("Viewing 11 - 11 of 11")
        browser.$("#prev_page").click
        browser.pageSource must contain("Viewing 1 - 10 of 11")
      }

      // GET /item/:id
      "accessing an item works does not show mets, download, or deposit" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        item_factory(1)
        browser.goTo("http://localhost:" + port + "/item/1")
        assertThat(browser.title()).isEqualTo("Item - TopicHub")
        browser.pageSource must not contain("View METS »")
        browser.pageSource must not contain("Download »")
        browser.pageSource must not contain("Deposit »")
        browser.pageSource must not contain("Create a Channel to enable Deposits")
      }

      // GET /item/package/:id
      "generating an item package works" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        skipped("Need to consider how to actually test this.")
      }

      // GET /item/deposit/:id
      "depositing an item redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        item_factory(1)
        val action = route(FakeRequest(GET, "/item/deposit/1")).get
        redirectLocation(action) must beSome.which(_ == "/login")
      }

      // GET /item/mets/:id
      "generating mets works" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        item_factory(1)
        val action = route(FakeRequest(GET, "/item/mets/1")).get
        status(action) must equalTo(OK)
        contentType(action) must beSome.which(_ == "application/xml")
      }

      // GET /items/missingtopics
      "viewing items in error state is denied" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        item_factory(1)
        val action = route(FakeRequest(GET, "/items/missingtopics")).get
        redirectLocation(action) must beSome.which(_ == "/login")
      }
    }

    "as a signed in user with no subscriber affiliated" should {
      // GET /item/deposit/:id
      "depositing an item redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub", "sub@example.com", "", "sub_identity")
        Subscriber.make(sub_user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        val user = create_user("current_user")
        item_factory(1)
        val action = route(FakeRequest(GET, "/item/deposit/1").
                           withSession(("connected", user.identity))).get
        contentAsString(action) must contain ("Reason: You are not a Subscriber")
      }.pendingUntilFixed(": currently we only support one subscriber so this works. See https://github.com/MITLibraries/scoap3hub/issues/46")

      // GET /item/:id
      "accessing an item works does not show mets, download or deposit" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        item_factory(1)
        browser.goTo("http://localhost:" + port + "/item/1")
        assertThat(browser.title()).isEqualTo("Item - TopicHub")
        browser.pageSource must not contain("View METS »")
        browser.pageSource must not contain("Download »")
        browser.pageSource must not contain("Deposit »")
        browser.pageSource must not contain("Create a Channel to enable Deposits")
      }
    }

    "as a signed in user with a subscriber affiliated" should {
      // GET /item/:id
      "accessing an item with no channel defined does not show mets, download or deposit" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("sub", "sub@example.com", "", "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        item_factory(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/item/1")
        assertThat(browser.title()).isEqualTo("Item - TopicHub")
        browser.pageSource must not contain("View METS »")
        browser.pageSource must not contain("Download »")
        browser.pageSource must not contain("Deposit »")
        browser.pageSource must contain("Create a Channel to enable Deposits")
      }

      "accessing an item with a channel defined shows deposit, but no mets or download" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("sub", "sub@example.com", "", "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact", Some("link"),
                                  Some("logo"))
        val ch = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                              "password", "http://example.com")
        item_factory(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/item/1")
        assertThat(browser.title()).isEqualTo("Item - TopicHub")
        browser.pageSource must not contain("View METS »")
        browser.pageSource must not contain("Download »")
        browser.pageSource must contain("Deposit »")
        browser.pageSource must not contain("Create a Channel to enable Deposits")
      }

      // GET /item/deposit/:id
      "depositing an item redirects to error with no channel defined" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("sub", "sub@example.com", "", "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        item_factory(1)
        val action = route(FakeRequest(GET, "/item/deposit/1").
                           withSession(("connected", user.identity),
                                       ("subscriber", sub.id.toString))).get
        status(action) must throwA[RuntimeException](message = "You must define a Channel")
      }

      // GET /item/deposit/:id
      "depositing an item works with a channel defined" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        skipped("Need to consider how to actually test this.")
      }
    }

    "as an analyst" should {
      // GET /item/:id
      "accessing an shows mets and download but not deposit" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.make("user", "user@example.com", "analyst", "https://oidc.mit.edu/current_user")
        val sub_user = User.make("sub", "sub@example.com", "analyst",
                                 "https://oidc.mit.edu/sub_user")
        Subscriber.make(sub_user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        item_factory(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/item/1")
        assertThat(browser.title()).isEqualTo("Item - TopicHub")
        browser.pageSource must contain("View METS »")
        browser.pageSource must contain("Download »")
        browser.pageSource must not contain("Deposit »")
        browser.pageSource must not contain("Create a Channel to enable Deposits")
      }

      // GET /items/missingtopics
      "viewing items in error state is denied" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.make("user", "user@example.com", "analyst", "https://oidc.mit.edu/current_user")
        item_factory(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/items/missingtopics")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      "deleting an item is denied" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.make("user", "user@example.com", "analyst", "https://oidc.mit.edu/current_user")
        item_factory(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/item/delete/1")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }
    }

    "as a sysadmin" should {
      // GET /items/missingtopics
      "viewing items in error state is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.make("user", "user@example.com", "sysadmin", "https://oidc.mit.edu/current_user")
        item_factory(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/items/missingtopics")
        assertThat(browser.title()).isEqualTo("Items missing Topics - TopicHub")
      }

      "reharvest link displays for Items from a Publisher with a single Harvest" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.make("user", "user@example.com", "sysadmin", "https://oidc.mit.edu/current_user")
        item_factory(1)
        Harvest.create(1, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/items/missingtopics")
        browser.pageSource must contain("Attempt Reharvest")
        browser.pageSource must contain("reharvest-1")
        browser.pageSource must not contain("Unable to determine Harvest")
      }

      "reharvest link not shown for Items from a Publisher with multiple Harvests" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.make("user", "user@example.com", "sysadmin", "https://oidc.mit.edu/current_user")
        item_factory(1)
        Harvest.create(1, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        Harvest.create(1, "name2", "protocol", "http://www.example.com", "http://example.org", 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/items/missingtopics")
        browser.pageSource must not contain("Attempt Reharvest")
        browser.pageSource must not contain("reharvest-1")
        browser.pageSource must contain("Unable to determine Harvest")
      }

      "reharvest link not shown for Items from a Publisher with no Harvests" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.make("user", "user@example.com", "sysadmin", "https://oidc.mit.edu/current_user")
        item_factory(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/items/missingtopics")
        browser.pageSource must not contain("Attempt Reharvest")
        browser.pageSource must not contain("reharvest-1")
        browser.pageSource must contain("Unable to determine Harvest")
      }

      "deleting an item is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.make("user", "user@example.com", "sysadmin", "https://oidc.mit.edu/current_user")
        item_factory(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/items/missingtopics")
        browser.$("#delete-1").click
        browser.pageSource must contain("Item deleted")
      }
    }
  }
}
