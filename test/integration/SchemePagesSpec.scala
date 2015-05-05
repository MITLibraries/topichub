import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ User, Scheme }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class SchemePagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")

  "Scheme pages" should {
    "as an unauthenticated User" should {

      // GET /schemes
      "accessing index displays schemes" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s1 = Scheme.make("scheme_1_tag", "gentype", "cat", "scheme_1 description",
                             Some("link"), Some("logo"))
        val s2 = Scheme.make("scheme_2_tag", "gentype", "cat", "scheme_2 description",
                             Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/schemes")
        assertThat(browser.title()).isEqualTo("Schemes - TopicHub")
        browser.pageSource must contain(s1.description)
        browser.pageSource must contain(s2.description)
        browser.pageSource must not contain("create a new scheme on this hub")
      }

      // GET /scheme/:id
      "viewing scheme displays scheme" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s1 = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s1.id)
        assertThat(browser.title()).isEqualTo("Scheme - TopicHub")
        browser.pageSource must contain(s1.description)
        browser.pageSource must not contain("manage scheme")
        browser.pageSource must contain("No topics")
      }

      // GET /scheme/:id/edit
      "accessing edit scheme form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s1 = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s1.id + "/edit")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /schemes/create
      "accessing new scheme create form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/schemes/create")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // POST /schemes
      "posting to scheme create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val action = route(FakeRequest(POST, "/schemes")).get
        redirectLocation(action) must beSome.which(_ == "/login")
        Scheme.all.size must equalTo(0)
      }
    }

    "as a User lacking the Analyst role" should {

      // GET /schemes
      "accessing index displays schemes" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val s1 = Scheme.make("scheme_1_tag", "gentype", "cat", "scheme_1 description",
                             Some("link"), Some("logo"))
        val s2 = Scheme.make("scheme_2_tag", "gentype", "cat", "scheme_2 description",
                             Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/schemes")
        assertThat(browser.title()).isEqualTo("Schemes - TopicHub")
        browser.pageSource must contain(s1.description)
        browser.pageSource must contain(s2.description)
        browser.pageSource must not contain("create a new scheme on this hub")
      }

      // GET /scheme/:id
      "viewing scheme displays scheme" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val s1 = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/scheme/" + s1.id)
        assertThat(browser.title()).isEqualTo("Scheme - TopicHub")
        browser.pageSource must contain(s1.description)
        browser.pageSource must not contain("manage scheme")
        browser.pageSource must contain("No topics")

      }

      // GET /scheme/:id/edit
      "accessing edit scheme form redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val s1 = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/scheme/" + s1.id + "/edit")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /schemes/create
      "accessing new scheme create form redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/schemes/create")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // POST /schemes
      "posting to scheme create redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("somerole")
        val action = route(FakeRequest(POST, "/schemes").
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        Scheme.all.size must equalTo(0)
      }
    }

    "as a User with the Analyst role" should {

      // GET /schemes
      "accessing index displays schemes" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("analyst")
        val s1 = Scheme.make("scheme_1_tag", "gentype", "cat", "scheme_1 description",
                             Some("link"), Some("logo"))
        val s2 = Scheme.make("scheme_2_tag", "gentype", "cat", "scheme_2 description",
                             Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/schemes")
        assertThat(browser.title()).isEqualTo("Schemes - TopicHub")
        browser.pageSource must contain(s1.description)
        browser.pageSource must contain(s2.description)
        browser.pageSource must contain("create a new scheme on this hub")
      }

      // GET /scheme/:id
      "viewing scheme displays scheme" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("analyst")
        val s1 = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/scheme/" + s1.id)
        assertThat(browser.title()).isEqualTo("Scheme - TopicHub")
        browser.pageSource must contain(s1.description)
        browser.pageSource must contain("manage scheme")
        browser.pageSource must contain("No topics")
      }

      // GET /scheme/:id/edit
      "accessing edit scheme displays scheme edit view" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        val s1 = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/scheme/" + s1.id + "/edit")
        assertThat(browser.title()).isEqualTo("Scheme - TopicHub")
        browser.pageSource must contain("New Validator")
        browser.pageSource must contain("New Finder")
      }

      // GET /schemes/create
      "accessing new scheme create form displays form" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/schemes/create")
        assertThat(browser.title()).isEqualTo("Create Scheme - TopicHub")
        browser.pageSource must contain("""<form action="/schemes" method="POST">""")
      }

      // POST /schemes
      "posting to scheme create creates scheme" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/schemes/create")
        browser.$("#tag").text("scheme_one_tag")
        browser.$("#description").text("I'm a totally good description for a scheme.")
        browser.$("#category").text("scheme cat")
        browser.$("#gentype").text("scheme gentype")
        browser.$("#home").text("http://www.example.com")
        browser.$("#logo").text("http://www.example.com/icon.png")
        browser.$("#submit").click
        assertThat(browser.title()).isEqualTo("Schemes - TopicHub")
        browser.pageSource must contain("totally good description")
        Scheme.all.size must equalTo(1)
      }
    }
  }
}
