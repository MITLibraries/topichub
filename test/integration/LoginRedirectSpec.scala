import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Subscriber, User }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class LoginRedirectSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")

  "Login Redirect" should {
    "as a user with a single affiliated Subscriber" should {
      "redirects to Subscriber Dashboard" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        assertThat(browser.title()).isEqualTo("Subscriber Dashboard - TopicHub")
      }
    }

    "as a user with multiple affiliated Subscriber" should {
      "redirects to Subscriber choose page" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        skipped(": implement with #46")
      }
    }

    "as a user with no affiliated Subscriber" should {
      "redirects to Subscriber Create with no note about joining group if no existing groups" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        assertThat(browser.title()).isEqualTo("Create Subscriber - TopicHub")
        browser.pageSource must not contain("You may also request to join an existing")
      }

      "redirects to Subscriber Create displaying note about joining group if groups exist" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("")
        val sub_user = User.make("subname", "sub@example.com", "",
                                 "https://oidc.mit.edu/another_user")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        assertThat(browser.title()).isEqualTo("Create Subscriber - TopicHub")
        browser.pageSource must contain("You may also request to join an existing group.")
      }
    }

    "as an analyst with no affiliated Subscriber" should {
      "redirects to Subscriber Create or Request to Join interstitial" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        assertThat(browser.title()).isEqualTo("Workbench - TopicHub")
      }
    }

    "as an analyst with an affiliated Subscriber" should {
      "redirects to Subscriber Create or Request to Join interstitial" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("analyst")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        assertThat(browser.title()).isEqualTo("Subscriber Dashboard - TopicHub")
      }
    }
  }
}
