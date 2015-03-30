import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._
import org.specs2.mock._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat

/**
 * add your integration spec here.
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class AuthenticationSpec extends Specification with Mockito {
  "Application" should {
    "display a login screen" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/login")
      browser.pageSource must contain("Log in with your MIT ID")
      assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
    }

    "redirect when login is clicked" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click();
      assertThat(browser.title()).isEqualTo("MIT OpenID Connect Pilot - Log In")
    }
  }
}
