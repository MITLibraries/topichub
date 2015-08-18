import org.specs2.mutable._
import org.specs2.runner._
import java.util.Date
import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import models.{ Collection, ContentType, Harvest, Publisher, ResourceMap, User, Subscriber }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class HarvestPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")
  def make_subscriber(userid: Int) = Subscriber.make(userid, "Sub Name", "cat", "contact",
                                                     Some("link"), Some("logo"))

  "Harvest pages" should {
    "as an unauthenticated User" should {
      // GET /publisher/:id/createH
      "accessing create form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/createH")
        browser.pageSource must contain(Play.configuration.getString("auth.login_text").get)
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      // POST /harvest/:id (publisher ID)
      "posting to create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val action = route(FakeRequest(POST, "/harvest/" + pub.id)).get
        redirectLocation(action) must beSome.which(_ == "/login")
        pub.harvestCount must equalTo(0)
      }

      // GET /harvest/:id
      "viewing harvest page redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        browser.goTo("http://localhost:" + port + "/harvest/" + harvest.id)
        browser.pageSource must contain(Play.configuration.getString("auth.login_text").get)
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      // GET /harvest/:id/start
      "accessing harvest start page redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        browser.goTo("http://localhost:" + port + "/harvest/" + harvest.id + "/start?span=1")
        Harvest.findById(harvest.id).get.updated must equalTo(harvest.updated)
        browser.pageSource must contain(Play.configuration.getString("auth.login_text").get)
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      //GET /harvest/:id/delete
      "accessing harvest delete redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        browser.goTo("http://localhost:" + port + "/harvest/" + harvest.id + "/delete")
        Harvest.findById(harvest.id).get must equalTo(harvest)
        browser.pageSource must contain(Play.configuration.getString("auth.login_text").get)
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      // GET /knip/:cid/:hid/:oid
      "accessing harvest knip feature redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        var collection = Collection.make(pub.id, ct.id, rm.id, "coll", "desc", "open")
        browser.goTo("http://localhost:" + port + "/knip/" + collection.id + "/" + harvest.id +
                     "/scoap:asdf:fdsa")
        browser.pageSource must contain(Play.configuration.getString("auth.login_text").get)
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }
    }

    "as a User unaffiliated with the Publisher" should {
      // GET /publisher/:id/createH
      "accessing create form redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/createH")
        browser.pageSource must contain("Reason: You are not authorized")
      }

      // POST /harvest/:id (publisher ID)
      "posting to create redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val action = route(FakeRequest(POST, "/harvest/" + pub.id).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        pub.harvestCount must equalTo(0)
      }

      // GET /harvest/:id
      "viewing harvest page redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/harvest/" + harvest.id)
        browser.pageSource must contain("Reason: You are not authorized")
      }

      // GET /harvest/:id/start
      "accessing harvest start page redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/harvest/" + harvest.id + "/start?span=1")
        Harvest.findById(harvest.id).get.updated must equalTo(harvest.updated)
        browser.pageSource must contain("Reason: You are not authorized")
      }

      // GET /harvest/:id/delete
      "accessing harvest delete redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/harvest/" + harvest.id + "/delete")
        Harvest.findById(harvest.id).get must equalTo(harvest)
        browser.pageSource must contain("Reason: You are not authorized")
      }

      // GET /knip/:cid/:hid/:oid
      "accessing harvest knip feature redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        var collection = Collection.make(pub.id, ct.id, rm.id, "coll", "desc", "open")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/knip/" + collection.id + "/" + harvest.id +
                     "/scoap:asdf:fdsa")
        browser.pageSource must contain("Reason: You are not authorized")
      }
    }

    "as a User affiliated with the Publisher" should {
      // GET /publisher/:id/createH
      "accessing create form succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/createH")
        assertThat(browser.title()).isEqualTo("Harvest - TopicHub")
        browser.pageSource must contain("Define a content harvest")
      }

      // POST /harvest/:id (publisher ID)
      "posting to create succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        val pubHarvestCount = pub.harvestCount
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/createH")
        browser.$("#name").text("harvest_name")
        browser.$("#protocol").text("protocol")
        browser.$("#service_url").text("http://www.example.com/oai2d")
        browser.$("#resource_url").text("http://www.example.com/record/${recordId}/")
        browser.$("#submit").click
        browser.pageSource must contain("Valid date required")

        browser.$("#start").text("2015-01-01")
        browser.$("#submit").click
        assertThat(browser.title()).isEqualTo("Publisher - TopicHub")
        pub.harvestCount must equalTo(pubHarvestCount + 1)
      }

      // GET /harvest/:id
      "viewing harvest page succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/harvest/" + harvest.id)
        assertThat(browser.title()).isEqualTo("Harvest - TopicHub")
        browser.pageSource must contain("""<form action="/harvest/1/start" method="GET">""")
      }

      // GET /harvest/:id/start
      "accessing harvest start page succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/harvest/" + harvest.id + "/start?span=1")
        Harvest.findById(harvest.id).get.updated.toInstant must equalTo(harvest.updated.toInstant.plusSeconds(86400))
        assertThat(browser.title()).isEqualTo("Harvest - TopicHub")
        browser.pageSource must contain("""<form action="/harvest/1/start" method="GET">""")
      }

      //GET /harvest/:id/delete
      "accessing harvest delete succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/harvest/" + harvest.id + "/delete")
        Harvest.findById(harvest.id) must equalTo(None)
        assertThat(browser.title()).isEqualTo("Publisher - TopicHub")
        browser.pageSource must contain("/publisher/1/edit")
      }

      // GET /knip/:cid/:hid/:oid
      "accessing harvest knip feature succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        val harvest = Harvest.make(pub.id, "name", "protocol", "http://www.example.com",
                                   "http://example.org", 1, new Date)
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        var collection = Collection.make(pub.id, ct.id, rm.id, "coll", "desc", "open")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/knip/" + collection.id + "/" + harvest.id +
                     "/scoap:asdf:fdsa")
        assertThat(browser.title()).isEqualTo("TopicHub")
      }
    }
  }
}
