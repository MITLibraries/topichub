import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._
import org.specs2.mock._
import java.util.Date

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import models.{ Harvest, Publisher, Subscriber, User }

/**
 * add your integration spec here.
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class AuthenticationPageSpec extends Specification with Mockito {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")
  def make_subscriber(userid: Int) = Subscriber.make(userid, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

  "Application" should {
    "display a login screen" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/login")
      browser.pageSource must contain("Log in with your MIT ID")
      assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
    }

    "Analyst protected pages" should {
      "prompt for login when not signed in" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/workbench")
        browser.pageSource must contain("Log in with your MIT ID")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      "deny access when signed in without analyst role" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/workbench")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      "allow access when signed in with analyst role" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/workbench")
        browser.pageSource must contain("Analytics Workbench")
        assertThat(browser.title()).isEqualTo("Workbench - TopicHub")
      }

      "allow access when signed in with analyst role and additional roles" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("analyst, schmuck")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/workbench")
        browser.pageSource must contain("Analytics Workbench")
        assertThat(browser.title()).isEqualTo("Workbench - TopicHub")
      }
    }

    "Login protected pages" should {
      "prompt for login when not signed in" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/dashboard")
        browser.pageSource must contain("Log in with your MIT ID")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      "create user if authentication passes and no local user exists" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/dashboard")
        User.findByIdentity("https://oidc.mit.edu/current_user") must equalTo(None)
        browser.$("#openid").click
        assertThat(browser.title()).isEqualTo("Create Subscriber - TopicHub")
        User.findByIdentity("https://oidc.mit.edu/current_user") must not equalTo(None)
      }

      "allow access when signed in" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub = make_subscriber(user.id)
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val h1 = Harvest.make(pub.id, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/dashboard")
        assertThat(browser.title()).isEqualTo("Subscriber Dashboard - TopicHub")
      }
    }

  }
}
