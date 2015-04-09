import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ ContentType, Scheme, User }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class ContentTypePagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role, "current_user")

  "Content Type pages" should {
    "as an unauthenticated User" should {

      // GET /ctypes
      "accessing index redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/ctypes")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /ctype/:id
      "accessing content type show redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        browser.goTo("http://localhost:" + port + "/ctype/" + ct.id)
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /ctypes/create
      "accessing content type create form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/ctypes/create")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // POST /ctypes
      "posting to content type create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val action = route(FakeRequest(POST, "/ctypes")).get
        redirectLocation(action) must beSome.which(_ == "/login")
        ContentType.all.size must equalTo(0)
      }

      // POST /ctype/:id/scheme
      "posting to add content type scheme redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/ctype/" + ct.id + "/scheme?relation=relation")).get
        redirectLocation(action) must beSome.which(_ == "/login")
      }

      // GET /ctype/:id/scheme
      "accessing remove Content Type Scheme redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        ct.addScheme(s, "relation")
        ct.schemes("relation") must haveSize(1)
        browser.goTo("http://localhost:" + port + "/ctype/" + ct.id +
                     "/scheme?relation=relation&sid=" + s.id)
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
        ct.schemes("relation") must haveSize(1)
      }
    }

    "as a User lacking the Analyst role" should {

      // GET /ctypes
      "accessing index redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        browser.goTo("http://localhost:" + port + "/ctypes")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /ctype/:id
      "accessing content type show redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        browser.goTo("http://localhost:" + port + "/ctype/" + ct.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /ctypes/create
      "accessing content type create form redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        browser.goTo("http://localhost:" + port + "/ctypes/create")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // POST /ctypes
      "posting to content type create redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val action = route(FakeRequest(POST, "/ctypes")).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        ContentType.all.size must equalTo(0)
      }

      // POST /ctype/:id/scheme
      "posting to add content type scheme redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/ctype/" + ct.id + "/scheme?relation=relation")).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }

      // GET /ctype/:id/scheme
      "accessing remove Content Type Scheme redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        ct.addScheme(s, "relation")
        ct.schemes("relation") must haveSize(1)
        browser.goTo("http://localhost:" + port + "/ctype/" + ct.id +
                     "/scheme?relation=relation&sid=" + s.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
        ct.schemes("relation") must haveSize(1)
      }
    }

    "as a User with the Analyst role" should {

      // GET /ctypes
      "accessing index displays index" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/ctypes")
        assertThat(browser.title()).isEqualTo("Content Types - TopicHub")
      }

      // GET /ctype/:id
      "accessing content type show displays content type page" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        browser.goTo("http://localhost:" + port + "/ctype/" + ct.id)
        assertThat(browser.title()).isEqualTo("Content Type - TopicHub")
        browser.pageSource must contain(s"Content Type: ${ct.tag}")
      }

      // GET /ctypes/create
      "accessing content type create form displays form" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/ctypes/create")
        assertThat(browser.title()).isEqualTo("New Content Type - TopicHub")
        browser.pageSource must contain("""<form action="/ctypes" method="POST">""")
      }

      // POST /ctypes
      "posting to content type create creates a content type" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/ctypes/create")

        // submit without filling out required elements
        browser.$("#submit").click
        browser.pageSource must contain("This field is required")
        ContentType.all.size must equalTo(0)

        // submit with required elements
        browser.$("#tag").text("ct tag")
        browser.$("#label").text("label thing")
        browser.$("#description").text("super good description")
        browser.$("#logo").text("logo")
        browser.$("#submit").click
        assertThat(browser.title()).isEqualTo("Content Types - TopicHub")
        browser.pageSource must contain("super good description")
        ContentType.all.size must equalTo(1)
      }

      // POST /ctype/:id/scheme
      "posting to add content type scheme creates link" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        val ct = ContentType.make("c_tag", "label", "desc", Some("logo"))
        val s = Scheme.make("abstract_tag", "gentype", "cat", "s_desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/ctype/" + ct.id)
        ct.schemes("meta") must haveSize(0)
        ct.schemes("topic") must haveSize(0)
        ct.schemes("index") must haveSize(0)

        browser.$("#metadata_scheme_submit").click
        ct.schemes("meta") must haveSize(1)
        ct.schemes("topic") must haveSize(0)
        ct.schemes("index") must haveSize(0)

        browser.$("#topic_scheme_submit").click
        ct.schemes("meta") must haveSize(1)
        ct.schemes("topic") must haveSize(1)
        ct.schemes("index") must haveSize(0)

        browser.$("#index_scheme_submit").click
        ct.schemes("meta") must haveSize(1)
        ct.schemes("topic") must haveSize(1)
        ct.schemes("index") must haveSize(1)
      }

      // GET /ctype/:id/scheme
      "accessing remove Content Type Scheme removes link" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        val ct = ContentType.make("tag", "label", "desc", Some("logo"))
        val s = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        ct.addScheme(s, "relation")
        ct.schemes("relation") must haveSize(1)
        browser.goTo("http://localhost:" + port + "/ctype/" + ct.id +
                     "/scheme?relation=relation&sid=" + s.id)
        assertThat(browser.title()).isEqualTo("Content Type - TopicHub")
        ct.schemes("relation") must haveSize(0)
      }
    }
  }
}
