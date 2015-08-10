import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Channel, User, Subscriber }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class ChannelPagesSpec extends Specification {

  "Channel pages" should {
    "as an unauthenticated User" should {

      // GET /channel/:id
      "channel show redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("bob", "bob@example.com", "", "not_current_user")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val ch = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                              "password", "http://example.com")
        browser.goTo("http://localhost:" + port + "/channel/" + ch.id)
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // GET /channels/:sid/create
      "new channel form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("bob", "bob@example.com", "", "not_current_user")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/channels/" + sub.id + "/create")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // POST /subscriber/:sid/channels
      "channel create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("bob", "bob@example.com", "", "not_current_user")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/subscriber/" + sub.id + "/channels" )).get
        redirectLocation(action) must beSome.which(_ == "/login")
      }
    }

    "as a User with no associated Subscriber" should {

      // GET /channel/:id
      "channel show redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.make("bob", "bob@example.com", "", "current_user")
        val sub_user = User.make("subuser", "sub@example.com", "", "not_current_user")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val ch = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                              "password", "http://example.com")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/channel/" + ch.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /channels/:sid/create
      "new channel form redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.make("bob", "bob@example.com", "", "https://oidc.mit.edu/current_user")
        val sub_user = User.make("subuser", "sub@example.com", "", "not_current_user")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/channels/" + sub.id + "/create")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // POST /subscriber/:sid/channels
      "channel create redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.make("bob", "bob@example.com", "", "https://oidc.mit.edu/current_user")
        val sub_user = User.make("subuser", "sub@example.com", "", "not_current_user")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/subscriber/" + sub.id + "/channels" ).
                      withSession(("connected", "https://oidc.mit.edu/current_user"))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }
    }

    "as a User with an associated Subscriber" should {

      // GET /channel/:id
      "channel show is allowed for owned channel" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("bob", "bob@example.com", "", "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val ch = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                              "password", "http://example.com")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/channel/" + ch.id)
        assertThat(browser.title()).isEqualTo("Destination - TopicHub")
        browser.pageSource must contain(s"URL: ${ch.channelUrl}")
      }

      // GET /channel/:id
      "channel show redirects to trouble for non owned channel" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("bob", "bob@example.com", "", "https://oidc.mit.edu/current_user")
        val user_sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val user_ch = Channel.make(user_sub.id, "protocol", "mode", "description", "userid",
                              "password", "http://example.com")
        val another_user = User.make("auser", "another@example.com", "", "not_current_user")
        val sub = Subscriber.make(another_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val ch = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                              "password", "http://example.com")

        // can view own channel
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/channel/" + user_ch.id)
        assertThat(browser.title()).isEqualTo("Destination - TopicHub")

        // cannot view someone else's channel
        browser.goTo("http://localhost:" + port + "/channel/" + ch.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /channels/:sid/create
      "new channel form is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user", "user@example.com", "", "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/channels/" + sub.id + "/create")
        assertThat(browser.title()).isEqualTo("New Destination - TopicHub")
        browser.pageSource must contain(s"""<form action="/subscriber/${sub.id}/channels" method="POST""")
      }

      // POST /subscriber/:sid/channels
      "channel create is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user", "user@example.com", "", "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/channels/" + sub.id + "/create")

        Channel.findByUrl("http://example.com") must equalTo(None)
        // without required fields
        browser.$("#submit").click
        browser.pageSource must contain("This field is required")
        Channel.findByUrl("http://example.com") must equalTo(None)

        browser.$("#description").text("This is where I want stuff to go")
        browser.$("#userId").text("username")
        browser.$("#password").text("pwd")
        browser.$("#channelUrl").text("http://example.com")
        browser.$("#submit").click
        assertThat(browser.title()).isEqualTo("Subscriber Dashboard - TopicHub")
        val ch = Channel.findByUrl("http://example.com")
        ch must not equalTo(None)
        ch.get.description must equalTo("This is where I want stuff to go")
      }
    }
  }
}
