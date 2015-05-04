import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Channel, Plan, Scheme, User, Subscriber, Subscription, Topic }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class SubscriptionPagesSpec extends Specification {

  "Subscription pages" should {
    "as an unauthenticated User" should {

      // GET /subscriptions/browse?filter=scheme&value=scheme.id
      "browse redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        Subscription.create(sub.id, t.id, "deliver", sub.created, sub.created)
        browser.goTo("http://localhost:" + port + "/subscriptions/browse?filter=scheme&value=" + scheme.id)
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }
    }

    "as a User with no associated Subscriber" should {

      // GET /subscriptions/browse?filter=scheme&value=scheme.id
      "browse redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        Subscription.create(sub.id, t.id, "deliver", sub.created, sub.created)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/subscriptions/browse?filter=scheme&value="
                     + scheme.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }
    }

    "as a User with Subscriber" should {

      // GET /subscriptions/browse?filter=scheme&value=scheme.id
      "browse is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        Subscription.create(sub.id, t.id, "deliver", sub.created, sub.created)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/subscriptions/browse?filter=scheme&value="
                     + scheme.id)
        assertThat(browser.title()).isEqualTo("Subscription Browse - TopicHub")
      }
    }
  }
}
