import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ ContentFormat, User }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class ContentFormatPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role, "current_user")

  "Content Format pages" should {
    "as an unauthenticated User" should {

      // GET /cformats
      "accessing index redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/cformats")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /cformat/:id
      "accessing content format show redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val cf = ContentFormat.make("cf_tag", "cf_label", "cf_desc", "http://www.example.com",
                                    "mimetype", Some("logo"))
        browser.goTo("http://localhost:" + port + "/cformat/" + cf.id)
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /cformats/create
      "accessing content format create form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/cformats/create")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // POST /cformats
      "posting to content format create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val action = route(FakeRequest(POST, "/cformats")).get
        redirectLocation(action) must beSome.which(_ == "/login")
        ContentFormat.all.size must equalTo(0)
      }
    }

    "as a User lacking the Analyst role" should {

      // GET /cformats
      "accessing index redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        browser.goTo("http://localhost:" + port + "/cformats")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /cformat/:id
      "accessing content format show redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val cf = ContentFormat.make("cf_tag", "cf_label", "cf_desc", "http://www.example.com",
                                    "mimetype", Some("logo"))
        browser.goTo("http://localhost:" + port + "/cformat/" + cf.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /cformats/create
      "accessing content format create form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        browser.goTo("http://localhost:" + port + "/cformats/create")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // POST /cformats
      "posting to content format create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val action = route(FakeRequest(POST, "/cformats")).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        ContentFormat.all.size must equalTo(0)
      }
    }

    "as a User with the Analyst role" should {

      // GET /cformats
      "accessing index redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/cformats")
        assertThat(browser.title()).isEqualTo("Content Formats - TopicHub")
      }

      // GET /cformat/:id
      "accessing content format show redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        val cf = ContentFormat.make("cf_tag", "cf_label", "cf_desc", "http://www.example.com",
                                    "mimetype", Some("logo"))
        browser.goTo("http://localhost:" + port + "/cformat/" + cf.id)
        assertThat(browser.title()).isEqualTo("Content Format - TopicHub")
        browser.pageSource must contain(s"Content Format: ${cf.tag}")
      }

      // GET /cformats/create
      "accessing content format create form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/cformats/create")
        assertThat(browser.title()).isEqualTo("New Content Format - TopicHub")
        browser.pageSource must contain("""<form action="/cformats" method="POST">""")
      }

      // POST /cformats
      "posting to content format create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/cformats/create")
        ContentFormat.all.size must equalTo(0)
        // without required fields
        browser.$("#submit").click
        ContentFormat.all.size must equalTo(0)

        // with required fields
        browser.$("#tag").text("New Content Format Tag")
        browser.$("#label").text("New Content Format Label")
        browser.$("#description").text("Super Good Content Format Description!!!")
        browser.$("#url").text("http://example.com")
        browser.$("#mimetype").text("some/mimetype")
        browser.$("#logo").text("http://example.com/logo.png")
        browser.$("#submit").click
        ContentFormat.all.size must equalTo(1)
        assertThat(browser.title()).isEqualTo("Content Formats - TopicHub")
      }
    }
  }
}
