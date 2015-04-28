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
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class DashboardPagesSpec extends Specification with Mockito {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")
  def make_subscriber(userid: Int) = Subscriber.make(userid, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

  "The Dashboard" should {
    "prompts for login when not signed in" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/dashboard")
      browser.pageSource must contain("Log in with your MIT ID")
      assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
    }

    "allows access when signed in if a Subscriber is associated with the User" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      val sub = make_subscriber(user.id)
      val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
      val h1 = Harvest.make(pub.id, "name", "protocol", "http://www.example.com", "http://example.org", 1, new Date)

      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/dashboard")
      browser.pageSource must contain("Suggested Subscriptions")
      assertThat(browser.title()).isEqualTo("Subscriber Dashboard - TopicHub")
    }

    "denies access if no Subscriber is associated with the User" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")

      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/dashboard")
      browser.pageSource must contain("No Subscriber found for your User Account")
    }
  }
}
