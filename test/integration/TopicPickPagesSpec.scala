import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Agent, Collection, ContentType, Hold, Item, Publisher, ResourceMap,
                Scheme, User, Subscriber, Subscription, Topic, TopicPick }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class TopicPickPagesSpec extends Specification {

  "TopicPick pages" should {
    "as an unauthenticated User" should {

      // GET /picks/browse?id=sub.id&page=x
      "browse redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val a = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        TopicPick.create(sub.id, t.id, a.id)
        browser.goTo("http://localhost:" + port + "/picks/browse?id=" + sub.id)
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      // GET /pick/:id/resolve?accept=Boolean
      "resolve redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val a = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        val tp = TopicPick.make(sub.id, t.id, a.id)
        sub.pickCount must equalTo(1)
        browser.goTo("http://localhost:" + port + "/pick/" + tp.id + "/resolve?accept=true")
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
        sub.pickCount must equalTo(1)
      }
    }

    "as a User not accociated with Subscriber" should {

      // GET /picks/browse?id=sub.id&page=x
      "browse redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val user_sub = Subscriber.make(user.id, "User Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val a = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        TopicPick.create(sub.id, t.id, a.id)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/picks/browse?id=" + sub.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /pick/:id/resolve?accept=Boolean
      "resolve redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val user_sub = Subscriber.make(user.id, "User Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val a = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        val tp = TopicPick.make(sub.id, t.id, a.id)
        sub.pickCount must equalTo(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/pick/" + tp.id + "/resolve?accept=true")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
        sub.pickCount must equalTo(1)
      }
    }

    "as a User associated with Subscriber" should {

      // GET /picks/browse?id=sub.id&page=x
      "browse is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val a = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        TopicPick.create(sub.id, t.id, a.id)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/picks/browse?id=" + sub.id)
        assertThat(browser.title()).isEqualTo("Topic Picks Browse - TopicHub")
      }

      // GET /pick/:id/resolve?accept=Boolean
      "resolve with true removes pick" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("sub_name", "sub@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val a = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        val tp = TopicPick.make(sub.id, t.id, a.id)
        sub.pickCount must equalTo(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/pick/" + tp.id + "/resolve?accept=true")
        assertThat(browser.title()).isEqualTo("Topic Picks Browse - TopicHub")
        sub.pickCount must equalTo(0)
      }.pendingUntilFixed("see https://github.com/MITLibraries/scoap3hub/issues/170")

      // GET /pick/:id/resolve?accept=Boolean
      "resolve with false removes pick" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("sub_name", "sub@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val a = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        val tp = TopicPick.make(sub.id, t.id, a.id)
        sub.pickCount must equalTo(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/pick/" + tp.id + "/resolve?accept=false")
        assertThat(browser.title()).isEqualTo("Topic Picks Browse - TopicHub")
        sub.pickCount must equalTo(0)
      }.pendingUntilFixed("see https://github.com/MITLibraries/scoap3hub/issues/170")
    }
  }
}
