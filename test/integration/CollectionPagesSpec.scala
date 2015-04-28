import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Collection, ContentType, Harvest, Publisher, ResourceMap, User, Subscriber }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class CollectionPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")

  "Collection pages" should {
    "as an unauthenticated User" should {

      // GET /collections
      "accessing index is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        var collection1 = Collection.make(pub.id, ct.id, rm.id, "coll1_tag", "coll1 desc", "open")
        var collection2 = Collection.make(pub.id, ct.id, rm.id, "coll2_tag", "coll2 desc", "open")
        browser.goTo("http://localhost:" + port + "/collections")
        assertThat(browser.title()).isEqualTo("Collections - TopicHub")
        browser.pageSource must contain(collection1.description)
        browser.pageSource must contain(collection2.description)
      }

      // POST /publisher/:id
      "posting to create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val action = route(FakeRequest(POST, "/publisher/" + pub.id)).get
        redirectLocation(action) must beSome.which(_ == "/login")
        pub.collectionCount must equalTo(0)
      }

      // GET /publisher/:id/create
      "accessing new collection form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/create")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }
    }

    "as a User unaffiliated with the Publisher" should {

      // GET /collections
      "accessing index is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("somerole")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        var collection1 = Collection.make(pub.id, ct.id, rm.id, "coll1_tag", "coll1 desc", "open")
        var collection2 = Collection.make(pub.id, ct.id, rm.id, "coll2_tag", "coll2 desc", "open")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/collections")
        assertThat(browser.title()).isEqualTo("Collections - TopicHub")
        browser.pageSource must contain(collection1.description)
        browser.pageSource must contain(collection2.description)
      }

      // POST /publisher/:id
      "posting to create redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("somerole")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val action = route(FakeRequest(POST, "/publisher/" + pub.id).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        pub.collectionCount must equalTo(0)
      }

      // GET /publisher/:id/create
      "accessing new collection form redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("somerole")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/create")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("Reason: You are not authorized")
      }
    }

    "as a User affiliated with the Publisher" should {

      // GET /collections
      "accessing index is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("somerole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        var collection1 = Collection.make(pub.id, ct.id, rm.id, "coll1_tag", "coll1 desc", "open")
        var collection2 = Collection.make(pub.id, ct.id, rm.id, "coll2_tag", "coll2 desc", "open")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/collections")
        assertThat(browser.title()).isEqualTo("Collections - TopicHub")
        browser.pageSource must contain(collection1.description)
        browser.pageSource must contain(collection2.description)
      }

      // POST /publisher/:id
      "posting to create is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("somerole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val ct = ContentType.make("ct_tag_1", "ct_label_1", "desc", Some("logo"))
        val rm = ResourceMap.make("rm_tag_1", "rm desc", Some("swordurl"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/create")
        browser.$("#tag").text("collection_one_tag")
        browser.$("#description").text("I'm a totally good description for a collection.")
        browser.$("#policy").text("open access")
        browser.$("#submit").click
        pub.collectionCount must equalTo(1)
        assertThat(browser.title()).isEqualTo("Publisher - TopicHub")
        browser.pageSource must contain("totally good description")
      }

      // GET /publisher/:id/create
      "accessing new collection form is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("somerole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/create")
        assertThat(browser.title()).isEqualTo("New Collection - TopicHub")
        browser.pageSource must contain("Define a content collection")
      }
    }
  }
}
