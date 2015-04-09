import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ ContentFormat, ResourceMap, Scheme, User }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class ResourceMapPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role, "current_user")

  "Resource Map pages" should {
    "as an unauthenticated User" should {

      // GET /resmaps
      "accessing index page redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/resmaps")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /resmap/:id
      "accessing show page redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
        browser.goTo("http://localhost:" + port + "/resmap/" + rm.id)
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /resmaps/create
      "accessing new form page redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/resmaps/create")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // POST /resmaps
      "posting to create page redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val action = route(FakeRequest(POST, "/resmaps")).get
        redirectLocation(action) must beSome.which(_ == "/login")
        ResourceMap.all.size must equalTo(0)
      }

      // POST /resmap/:id
      "posting to add resource mapping redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("s_tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("cf_tag", "label", "desc", "http://www.example.com",
                                    "mimetype", Some("logo"))
        val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
        val action = route(FakeRequest(POST, "/resmap/" + rm.id)).get
        redirectLocation(action) must beSome.which(_ == "/login")
        rm.mappingsForScheme(s).size must equalTo(0)
      }

      // GET /resmap/:id/scheme
      "removing resource mapping redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("s_tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("cf_tag", "label", "desc", "http://www.example.com",
                                    "mimetype", Some("logo"))
        val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
        rm.addMapping(s.id, cf.id, "source", 1)
        rm.mappingsForScheme(s).size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/resmap/" + rm.id + "/scheme?sid=" +
                     s.id +"&source=source")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
        rm.mappingsForScheme(s).size must equalTo(1)
      }
    }

    "as a User lacking the Analyst role" should {

      // GET /resmaps
      "accessing index page redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        browser.goTo("http://localhost:" + port + "/resmaps")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /resmap/:id
      "accessing show page redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
        browser.goTo("http://localhost:" + port + "/resmap/" + rm.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /resmaps/create
      "accessing new form page redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        browser.goTo("http://localhost:" + port + "/resmaps/create")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // POST /resmaps
      "posting to create page redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val action = route(FakeRequest(POST, "/resmaps")).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        ResourceMap.all.size must equalTo(0)
      }

      // POST /resmap/:id
      "posting to add resource mapping redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val s = Scheme.make("s_tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("cf_tag", "label", "desc", "http://www.example.com",
                                    "mimetype", Some("logo"))
        val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
        val action = route(FakeRequest(POST, "/resmap/" + rm.id)).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        rm.mappingsForScheme(s).size must equalTo(0)
      }

      // GET /resmap/:id/scheme
      "removing resource mapping redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val s = Scheme.make("s_tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("cf_tag", "label", "desc", "http://www.example.com",
                                    "mimetype", Some("logo"))
        val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
        rm.addMapping(s.id, cf.id, "source", 1)
        rm.mappingsForScheme(s).size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/resmap/" + rm.id + "/scheme?sid=" +
                     s.id +"&source=source")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
        rm.mappingsForScheme(s).size must equalTo(1)
      }
    }

    "as a User with the Analyst role" should {

      // GET /resmaps
      "accessing index page is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/resmaps")
        assertThat(browser.title()).isEqualTo("Resource Maps - TopicHub")
      }

      // GET /resmap/:id
      "accessing show page is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
        browser.goTo("http://localhost:" + port + "/resmap/" + rm.id)
        assertThat(browser.title()).isEqualTo("Resource Map - TopicHub")
        browser.pageSource must contain(s"Resource Map: ${rm.tag}")
      }

      // GET /resmaps/create
      "accessing new form page is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/resmaps/create")
        assertThat(browser.title()).isEqualTo("New Resource Map - TopicHub")
        browser.pageSource must contain("""<form action="/resmaps" method="POST">""")
      }

      // POST /resmaps
      "posting to create page is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/resmaps/create")
        ResourceMap.all.size must equalTo(0)
        // without required fields
        browser.$("#submit").click
        ResourceMap.all.size must equalTo(0)

        // with required fields
        browser.$("#tag").text("New Resource Map Tag")
        browser.$("#description").text("Super Good Content Resource Map Description!!!")
        browser.$("#swordurl").text("http://example.com/sword_endpoint")
        browser.$("#submit").click
        ResourceMap.all.size must equalTo(1)
        assertThat(browser.title()).isEqualTo("Resource Maps - TopicHub")
      }

      // POST /resmap/:id
      "posting to add resource mapping is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        val s = Scheme.make("s_tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("cf_tag", "label", "desc", "http://www.example.com",
                                    "mimetype", Some("logo"))
        val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
        browser.goTo("http://localhost:" + port + "/resmap/" + rm.id)
        rm.mappingsForScheme(s).size must equalTo(0)
        // without required fields
        browser.$("#submit").click
        rm.mappingsForScheme(s).size must equalTo(0)
        // with required fields
        browser.$("#source").text("some source")
        browser.$("#submit").click
        rm.mappingsForScheme(s).size must equalTo(1)
      }

      // GET /resmap/:id/scheme
      "removing resource mapping is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        val s = Scheme.make("s_tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val cf = ContentFormat.make("cf_tag", "label", "desc", "http://www.example.com",
                                    "mimetype", Some("logo"))
        val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
        rm.addMapping(s.id, cf.id, "source", 1)
        rm.mappingsForScheme(s).size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/resmap/" + rm.id + "/scheme?sid=" +
                     s.id +"&source=source")
        assertThat(browser.title()).isEqualTo("Resource Map - TopicHub")
        rm.mappingsForScheme(s).size must equalTo(0)
      }
    }
  }
}
