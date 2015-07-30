import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._
import java.util.Date

import play.api.Play
import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import models.{ Subscriber, User }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class UserDashboardPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")
  "The User Dashboard" should {
    "prompts for login when not signed in" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/myaccount")
      browser.pageSource must contain(Play.configuration.getString("auth.login_text").get)
      assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
    }

    "Displays the current user's data when signed in" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/myaccount")
      browser.pageSource must contain(user.email)
      assertThat(browser.title()).isEqualTo("My Account - TopicHub")
    }

    "Allows user to switch Session Subscriber if they have multiple" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("")
      val s1 = Subscriber.make(user.id, "Sub1 Name", "cat", "contact", Some("link"), Some("logo"))
      val s2 = Subscriber.make(user.id, "Sub2 Name", "cat", "contact", Some("link"), Some("logo"))

      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/myaccount")
      browser.find("#sub_1").getText() must contain(s"${s1.name} (Current Subscriber)")
      browser.find("#sub_2").getText() must contain(s"${s2.name} (Act as this Subscriber)")

      browser.$("#sub_switch_2").click
      browser.find("#sub_1").getText() must contain(s"${s1.name} (Act as this Subscriber)")
      browser.find("#sub_2").getText() must contain(s"${s2.name} (Current Subscriber)")
    }

    "Deny User to switch to a Session Subscriber they are not affiliated with" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("")
      val sub_user = User.make("subuser", "sub@example.com", "role", "sub_identity")
      val s1 = Subscriber.make(user.id, "Sub1 Name", "cat", "contact", Some("link"), Some("logo"))
      val s2 = Subscriber.make(sub_user.id, "Sub2 Name", "cat", "contact", Some("link"), Some("logo"))
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/myaccount")
      browser.find("#sub_1").getText() must contain(s"${s1.name} (Current Subscriber)")
      browser.find("#sub_2").getText() must not contain(s"${s2.name} (Act as this Subscriber)")

      browser.goTo("http://localhost:" + port + "/subscriber/" + s2.id + "/updatesession")
      assertThat(browser.title()).isEqualTo("Error - TopicHub")
      browser.pageSource must contain("You are not authorized")

      browser.goTo("http://localhost:" + port + "/myaccount")
      browser.find("#sub_1").getText() must contain(s"${s1.name} (Current Subscriber)")
    }
  }
}
