import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import models.{ Interest, Scheme, Subscriber, User }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class SubscriberInterestPagesSpec extends Specification {
  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")
  def make_subscriber(userid: Int) =
    Subscriber.make(userid, "Sub Name", "cat","contact", Some("link"), Some("logo"))

  "SubscriberInterestPages pages" should {
    "as an unauthenticated User" should {
      // GET /interests/browse?filter=scheme&value=Institution
      "browse redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/interests/browse?filter=scheme&value=Institution")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      // POST /interests
      "adding new interest redirects to login"  in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val action = route(FakeRequest(POST, "/interests?scheme=popcorn")).get
        redirectLocation(action) must beSome.which(_ == "/login")
      }

      // GET /interests/remove/:iid
      "removing interest redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
        val sub = make_subscriber(sub_user.id)
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        i must equalTo(Interest.findById(1).get)
        browser.goTo("http://localhost:" + port + "/interests/remove/" + i.id)
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
        i must equalTo(Interest.findById(1).get)
      }
    }

    "as a User not accociated with any Subscriber" should {
      // GET /interests/browse?filter=scheme&value=Institution
      "browse redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/interests/browse?filter=scheme&value=Institution")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("Reason: You must have a Subscriber defined")
      }

      // POST /interests
      "adding new interest redirects to error"  in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        val action = route(FakeRequest(POST, "/interests?scheme=popcorn").
                           withSession(("connected", user.identity))).get
        contentAsString(action) must contain ("Reason: No such subscriber: 0")
      }

      // GET /interests/remove/:iid
      "removing interest redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
        val sub = make_subscriber(sub_user.id)
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        val user = create_user("")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        i must equalTo(Interest.findById(1).get)
        browser.goTo("http://localhost:" + port + "/interests/remove/" + i.id)
        i must equalTo(Interest.findById(1).get)
        browser.pageSource must contain("Reason: You are not authorized")
      }
    }

    "as a User associated with a Subscriber" should {
      // GET /interests/browse?filter=scheme&value=Institution
      "browse for valid scheme with no subscriptions" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        val sub = make_subscriber(user.id)
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/interests/browse?filter=scheme&value=tag")
        assertThat(browser.title()).isEqualTo("Interest Browse - TopicHub")
        browser.pageSource must contain("Viewing all 0 records.")
      }

      "browse for valid scheme with subscriptions" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        val sub = make_subscriber(user.id)
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Interest.make(sub.id, scheme.tag, "MIT", false)
        Interest.make(sub.id, scheme.tag, "MIT2", false)
        val scheme2 = Scheme.make("tag2", "gentype", "cat", "desc", Some("link"), Some("logo"))
        Interest.make(sub.id, scheme2.tag, "MIT", false)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/interests/browse?filter=scheme&value=tag")
        assertThat(browser.title()).isEqualTo("Interest Browse - TopicHub")
        browser.pageSource must contain("Viewing all 2 records.")
      }

      "browse for invalid scheme" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        val sub = make_subscriber(user.id)
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/interests/browse?filter=scheme&value=invalid")
        assertThat(browser.title()).isEqualTo("Interest Browse - TopicHub")
        browser.pageSource must contain("No match for supplied scheme: invalid")
      }

      // POST /interests
      "adding new interest for own subscriber"  in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        val sub = make_subscriber(user.id)
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/scheme/" + scheme.id)
        browser.$("#interest").text("match this text")
        browser.$("#submit").click
        assertThat(browser.title()).isEqualTo("Interest Browse - TopicHub")
        browser.pageSource must contain("Viewing all 1 records.")
      }

      // GET /interests/remove/:iid
      "removing interest for own subscriber" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        val sub = make_subscriber(user.id)
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        val i2 = Interest.make(sub.id, scheme.tag, "MIT2", false)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/interests/browse?filter=scheme&value=tag")
        assertThat(browser.title()).isEqualTo("Interest Browse - TopicHub")
        browser.pageSource must contain("Viewing all 2 records.")
        browser.$("#remove-" + i.id).click
        browser.pageSource must contain("Viewing all 1 records.")
      }

      // GET /interests/remove/:iid
      "removing template interest for own subscriber" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        val sub = make_subscriber(user.id)
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val i = Interest.make(sub.id, scheme.tag, "MIT", true)
        val i2 = Interest.make(sub.id, scheme.tag, "MIT2", true)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/interests/browse?filter=scheme&value=tag")
        assertThat(browser.title()).isEqualTo("Interest Browse - TopicHub")
        browser.pageSource must contain("Viewing all 2 records.")
        browser.$("#remove-" + i.id).click
        browser.pageSource must contain("Viewing all 1 records.")
      }

      // GET /interests/remove/:iid
      "removing interest redirects to error for other subscriber interests" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        val sub = make_subscriber(user.id)
        val another_user = User.make("another_user", "another@example.com", "",
                                     "https://oidc.mit.edu/another_user")
        val another_sub = Subscriber.make(another_user.id, "Sub Name2", "cat","contact", Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val i = Interest.make(sub.id, scheme.tag, "MIT", false)
        val i2 = Interest.make(another_sub.id, scheme.tag, "MIT2", false)
        sub.interests.size must equalTo(1)
        another_sub.interests.size must equalTo(1)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/interests/browse?filter=scheme&value=tag")
        browser.pageSource must contain("Viewing all 1 records.")

        browser.goTo("http://localhost:" + port + "/interests/remove/" + i.id)
        browser.pageSource must contain("Viewing all 0 records.")
        sub.interests.size must equalTo(0)
        another_sub.interests.size must equalTo(1)

        browser.goTo("http://localhost:" + port + "/interests/remove/" + i2.id)
        browser.pageSource must contain("Reason: You are not authorized")
        sub.interests.size must equalTo(0)
        another_sub.interests.size must equalTo(1)
      }
    }
  }
}
