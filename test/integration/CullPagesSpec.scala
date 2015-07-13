import org.specs2.mutable._
import org.specs2.runner._
import java.util.Date
import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Collection, ContentType, Cull, Publisher, ResourceMap, User, Subscriber }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class CullPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")
  def make_subscriber(userid: Int) = Subscriber.make(userid, "Sub Name", "cat", "contact",
                                                     Some("link"), Some("logo"))

  "Cull pages" should {
    "as an unauthenticated User" should {
      // GET /publisher/:id/createC
      "accessing create form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/createC")
        browser.pageSource must contain("Log in with your MIT ID")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // POST /cull/:id (publisher ID)
      "posting to create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val action = route(FakeRequest(POST, "/cull/" + pub.id)).get
        redirectLocation(action) must beSome.which(_ == "/login")
        pub.cullCount must equalTo(0)
      }

      // GET /cull/:id
      "viewing cull page redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        browser.goTo("http://localhost:" + port + "/cull/" + cull.id)
        browser.pageSource must contain("Log in with your MIT ID")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /cull/:id/start
      "accessing cull start page redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        browser.goTo("http://localhost:" + port + "/cull/" + cull.id + "/start?span=1")
        Cull.findById(cull.id).get.updated must equalTo(cull.updated)
        browser.pageSource must contain("Log in with your MIT ID")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      //GET /cull/:id/delete
      "accessing cull delete redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        browser.goTo("http://localhost:" + port + "/cull/" + cull.id + "/delete")
        Cull.findById(cull.id).get must equalTo(cull)
        browser.pageSource must contain("Log in with your MIT ID")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /knix/:cid/:oid
      "accessing cull knix feature redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        var collection = Collection.make(pub.id, ct.id, rm.id, "coll", "desc", "open")
        browser.goTo("http://localhost:" + port + "/knix/" + collection.id +
                     "/scoap:asdf:fdsa")
        browser.pageSource must contain("Log in with your MIT ID")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }
    }

    "as a User unaffiliated with the Publisher" should {
      // GET /publisher/:id/createC
      "accessing create form redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/createC")
        browser.pageSource must contain("Reason: You are not authorized")
      }

      // POST /cull/:id (publisher ID)
      "posting to create redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val action = route(FakeRequest(POST, "/cull/" + pub.id).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        pub.cullCount must equalTo(0)
      }

      // GET /cull/:id
      "viewing cull page redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/cull/" + cull.id)
        browser.pageSource must contain("Reason: You are not authorized")
      }

      // GET /cull/:id/start
      "accessing cull start page redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/cull/" + cull.id + "/start?span=1")
        Cull.findById(cull.id).get.updated must equalTo(cull.updated)
        browser.pageSource must contain("Reason: You are not authorized")
      }

      // GET /cull/:id/delete
      "accessing cull delete redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/cull/" + cull.id + "/delete")
        Cull.findById(cull.id).get must equalTo(cull)
        browser.pageSource must contain("Reason: You are not authorized")
      }

      // GET /knix/:cid/:oid
      "accessing cull knix feature redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        var collection = Collection.make(pub.id, ct.id, rm.id, "coll", "desc", "open")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/knix/" + collection.id +
                     "/scoap:asdf:fdsa")
        browser.pageSource must contain("Reason: You are not authorized")
      }
    }

    "as a User affiliated with the Publisher" should {
      // GET /publisher/:id/createC
      "accessing create form succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/createC")
        assertThat(browser.title()).isEqualTo("Cull - TopicHub")
        browser.pageSource must contain("Define a content cull")
      }

      // POST /cull/:id (publisher ID)
      "posting to create succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        val pubCullCount = pub.cullCount
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id + "/createC")
        browser.$("#name").text("cull_name")
        browser.$("#policy").text("soft")
        browser.$("#submit").click
        browser.pageSource must contain("Valid date required")

        browser.$("#start").text("2015-01-01")
        browser.$("#submit").click
        assertThat(browser.title()).isEqualTo("Publisher - TopicHub")
        pub.cullCount must equalTo(pubCullCount + 1)
      }

      // GET /cull/:id
      "viewing cull page succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/cull/" + cull.id)
        assertThat(browser.title()).isEqualTo("Cull - TopicHub")
        browser.pageSource must contain("""<form action="/cull/1/start" method="GET">""")
      }

      // GET /cull/:id/start
      "accessing cull start page succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/cull/" + cull.id + "/start?span=1")
        Cull.findById(cull.id).get.updated.toInstant must equalTo(cull.updated.toInstant.plusSeconds(86400))
        assertThat(browser.title()).isEqualTo("Cull - TopicHub")
        browser.pageSource must contain("""<form action="/cull/1/start" method="GET">""")
      }

      //GET /cull/:id/delete
      "accessing cull delete succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/cull/" + cull.id + "/delete")
        Cull.findById(cull.id) must equalTo(None)
        assertThat(browser.title()).isEqualTo("Publisher - TopicHub")
        browser.pageSource must contain("/publisher/1/edit")
      }

      // GET /knix/:cid/:oid - no defined page yet
      /*
      "accessing cull knix feature succeeds" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                                 Some("http://www.example.com"), Some(""))
        val cull = Cull.make(pub.id, "name", "soft", None, 1, new Date)
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val rm = ResourceMap.make("tag", "desc", Some("swordurl"))
        var collection = Collection.make(pub.id, ct.id, rm.id, "coll", "desc", "open")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/knix/" + collection.id +
                     "/scoap:asdf:fdsa")
        assertThat(browser.title()).isEqualTo("TopicHub")
      }
      */
    }
  }
}
