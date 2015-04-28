import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ ContentFormat, Finder, User, Scheme }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class FinderPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")

  "Finder pages" should {
    "as an unauthenticated User" should {

      // GET /scheme/:tag/finders
      "accessing index redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/finders")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /scheme/:tag/create
      "accessing new finder create form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/create")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // POST /scheme/:tag
      "posting to finder create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/scheme/" + s.tag)).get
        redirectLocation(action) must beSome.which(_ == "/login")
        Finder.findByScheme(s.id).size must equalTo(0)
      }

      // GET /scheme/:tag/finder/:id/delete
      "deleting finder redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        val f = Finder.make(s.id, cf.id, "description", "card", "idkey", "idlabel", "author")
        Finder.findByScheme(s.id).size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/finder/" + f.id + "/delete")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
        Finder.findByScheme(s.id).size must equalTo(1)
      }
    }

    "as a User lacking the Analyst role" should {

      // GET /scheme/:tag/finders
      "accessing index redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/finders")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /scheme/:tag/create
      "accessing new finder create form redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/create")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // POST /scheme/:tag
      "posting to finder create redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("somerole")
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/scheme/" + s.tag).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        Finder.findByScheme(s.id).size must equalTo(0)
      }

      // GET /scheme/:tag/finder/:id/delete
      "deleting finder redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        val f = Finder.make(s.id, cf.id, "description", "card", "idkey", "idlabel", "author")
        Finder.findByScheme(s.id).size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/finder/" + f.id + "/delete")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
        Finder.findByScheme(s.id).size must equalTo(1)
      }
    }

    "as a User with the Analyst role" should {

      // GET /scheme/:tag/finders
      "accessing index displays finders" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/finders")
        assertThat(browser.title()).isEqualTo("Finders - TopicHub")
        browser.pageSource must contain("Number of finders: 0")
      }

      // GET /scheme/:tag/create
      "accessing new finder create form displays form" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/create")
        assertThat(browser.title()).isEqualTo("Create Finder - TopicHub")
        browser.pageSource must contain(s"""<form action="/scheme/${s.tag}" method="POST">""")
      }

      // POST /scheme/:tag
      "posting to finder create creates finder" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        val s = Scheme.make("s_tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf1 = ContentFormat.make("cf_tag", "label", "desc", "http://www.example.com",
                                     "mimetype", Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/create")

        // submission without filling out fields
        browser.pageSource must not contain("This field is required")
        browser.$("#submit").click
        Finder.findByScheme(s.id).size must equalTo(0)
        browser.pageSource must contain("This field is required")

        // submission with valid fields
        browser.$("#description").text("finder description")
        browser.$("#cardinality").text("1")
        browser.$("#idKey").text("//some/xpath/statement")
        browser.$("#idLabel").text("No Label")
        browser.$("#author").text("Not Me")
        browser.$("#submit").click
        Finder.findByScheme(s.id).size must equalTo(1)
      }

      // GET /scheme/:tag/finder/:id/delete
      "deleting finder deletes finder" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("tag", "label", "desc", "http://www.example.com", "mimetype", Some("logo"))
        val f = Finder.make(s.id, cf.id, "description", "card", "idkey", "idlabel", "author")
        Finder.findByScheme(s.id).size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/finder/" + f.id + "/delete")
        Finder.findByScheme(s.id).size must equalTo(0)
      }
    }
  }
}
