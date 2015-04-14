import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ User, Subscriber }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class SubscriberPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role, "current_user")

  "Subscriber pages" should {
    "as an unauthenticated User" should {

      // GET /subscribers
      "index is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = create_user("not_current_user")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/subscribers")
        assertThat(browser.title()).isEqualTo("Subscribers - TopicHub")
        browser.pageSource must contain("(One subscriber)")
        browser.pageSource must contain("Sign Up »")
      }

      // GET /subscribers/browse
      "browsing is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("bob", "bob@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/subscribers/browse?filter=category&value=" + sub.category)
        assertThat(browser.title()).isEqualTo("Subscriber Browse - TopicHub")
        browser.pageSource must contain("""<a href="/subscriber/1">""")
      }

      // GET /subscribers/create
      "accessing new form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/subscribers/create")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // POST /subscribers
      "posting to create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val action = route(FakeRequest(POST, "/subscribers")).get
        redirectLocation(action) must beSome.which(_ == "/login")
      }

      // GET /subscriber/:id/edit
      "edit redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("bob", "bob@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/subscriber/" + sub.id + "/edit")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /subscriber/:id
      "view is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("bob", "bob@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/subscriber/" + sub.id)
        assertThat(browser.title()).isEqualTo("Subscriber - TopicHub")
      }
    }

    "as a User with no Associated Subscriber" should {

      // GET /subscribers/create
      "accessing new form is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("someone")
        browser.goTo("http://localhost:" + port + "/subscribers/create")
        assertThat(browser.title()).isEqualTo("Create Subscriber - TopicHub")
      }

      // POST /subscribers
      "posting to create is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("someone")
        browser.goTo("http://localhost:" + port + "/subscribers/create")

        // without required fields
        browser.$("#submit").click
        browser.pageSource must contain("This field is required")
        Subscriber.findByUserId(user.id) must equalTo(None)
        Subscriber.all.size must equalTo(0)

        // with required fields
        browser.$("#name").text("Some Subscriber Name")
        browser.$("#contact").text("sub@example.com")
        browser.$("#link").text("http://example.com")
        browser.$("#logo").text("http://example.com/logo.png")
        browser.$("#submit").click
        Subscriber.findByUserId(user.id) must not equalTo(None)
        Subscriber.all.size must equalTo(1)
      }

      // GET /subscriber/:id/edit
      "edit redirects to Error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        create_user("someone")
        browser.goTo("http://localhost:" + port + "/subscriber/" + sub.id + "/edit")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }
    }

    "as a User with an Associate Subscriber" should {
      // GET /subscribers/create
      "accessing new form is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("sub", "sub@example.com", "role", "current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/subscribers/create")
        assertThat(browser.title()).isEqualTo("Create Subscriber - TopicHub")
      }

      // POST /subscribers
      "posting to create is allowed (user can create multiple subscribers)" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("sub", "sub@example.com", "role", "current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/subscribers/create")
        Subscriber.all.size must equalTo(1)

        // without required fields
        browser.$("#submit").click
        browser.pageSource must contain("This field is required")
        Subscriber.all.size must equalTo(1)

        // with required fields
        browser.$("#name").text("Some Subscriber Name")
        browser.$("#contact").text("sub@example.com")
        browser.$("#link").text("http://example.com")
        browser.$("#logo").text("http://example.com/logo.png")
        browser.$("#submit").click
        Subscriber.all.size must equalTo(2)
      }

      // GET /subscriber/:id/edit
      "edit is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("sub", "sub@example.com", "role", "current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/subscriber/" + sub.id + "/edit")
        assertThat(browser.title()).isEqualTo("Edit Subscriber - TopicHub")
      }
    }
  }
}
