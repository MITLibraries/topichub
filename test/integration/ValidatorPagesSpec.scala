import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Scheme, User, Validator }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class ValidatorPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role, "current_user")

  "Validator pages" should {
    "as an unauthenticated User" should {

      // GET /scheme/:tag/createvalidator
      "accessing new validator form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/createvalidator")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // POST /scheme/:tag/validator
      "posting to validator create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/scheme/" + s.tag + "/validator")).get
        redirectLocation(action) must beSome.which(_ == "/login")
        Validator.findByScheme(s.id).size must equalTo(0)
      }

      // GET /scheme/:tag/validator/:id/delete
      "calling delete validator redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        Validator.create(s.id, "desc", "user", "pass", "servicecode", "serviceurl", "someone")
        val v = Validator.findByScheme(s.id).head
        Validator.findByScheme(s.id).size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/validator/" + v.id +
                     "/delete")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
        Validator.findByScheme(s.id).size must equalTo(1)
      }
    }

    "as a User lacking the Analyst role" should {

      // GET /scheme/:tag/createvalidator
      "accessing new validator form redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val s = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/createvalidator")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // POST /scheme/:tag/validator
      "posting to validator create redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val s = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/scheme/" + s.tag + "/validator")).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        Validator.findByScheme(s.id).size must equalTo(0)
      }

      // GET /scheme/:tag/validator/:id/delete
      "calling delete validator redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("somerole")
        val s = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        Validator.create(s.id, "desc", "user", "pass", "servicecode", "serviceurl", "someone")
        val v = Validator.findByScheme(s.id).head
        Validator.findByScheme(s.id).size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/validator/" + v.id +
                     "/delete")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
        Validator.findByScheme(s.id).size must equalTo(1)
      }
    }

    "as a User with the Analyst role" should {

      // GET /scheme/:tag/createvalidator
      "accessing new validator form displays form" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        val s = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/createvalidator")
        assertThat(browser.title()).isEqualTo("Create Validator - TopicHub")
        browser.pageSource must contain(
          s"""<form action="/scheme/${s.tag}/validator" method="POST">""")
      }

      // POST /scheme/:tag/validator
      "posting to validator create creates a validator" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        val s = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/createvalidator")

        // submit without required fields
        browser.pageSource must not contain("This field is required")
        browser.$("#submit").click
        browser.pageSource must contain("This field is required")
        Validator.findByScheme(s.id).size must equalTo(0)

        // fill out form
        browser.$("#description").text("I'm a totally good description for a validator")
        browser.$("#userId").text("user")
        browser.$("#password").text("password")
        browser.$("#serviceCode").text("some code")
        browser.$("#serviceUrl").text("http://www.example.com")
        browser.$("#author").text("not me")
        browser.$("#submit").click
        assertThat(browser.title()).isEqualTo("Finders - TopicHub")
        Validator.findByScheme(s.id).size must equalTo(1)
      }

      // GET /scheme/:tag/validator/:id/delete
      "calling delete validator deletes the validator" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        val s = Scheme.make("s1_tag", "gentype", "cat", "s1_desc", Some("link"), Some("logo"))
        Validator.create(s.id, "desc", "user", "pass", "servicecode", "serviceurl", "someone")
        val v = Validator.findByScheme(s.id).head
        Validator.findByScheme(s.id).size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/scheme/" + s.tag + "/validator/" + v.id +
                     "/delete")
        assertThat(browser.title()).isEqualTo("Finders - TopicHub")
        Validator.findByScheme(s.id).size must equalTo(0)
      }
    }
  }
}
