import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class StaticPagesSpec extends Specification {

  "home" should {
    "displays welcome message" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port)
      browser.pageSource must contain("Welcome to SCOAP")
      assertThat(browser.title()).isEqualTo("SCOAP3 - TopicHub")
    }

    "displays Schemes with counts" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      // load pubmodel and cmodel
      // counts should all be zero
      // add some items with topic schmes, counts should new values
      skipped
    }
  }

  "about" should {
    "display about message" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/about")
      browser.pageSource must contain("About TopicHub")
      assertThat(browser.title()).isEqualTo("About - TopicHub")
    }

    "link to github" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/about")
      browser.pageSource must contain("https://github.com/MITLibraries/scoap3hub")
    }
  }

  "feedback" should {
    "displays form" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/feedback")
      browser.pageSource must contain("Feedback")
      assertThat(browser.title()).isEqualTo("Feedback - TopicHub")
    }

    "submitting without required fields retains filled out fields" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/feedback")
      browser.$("#email").text("popcorn@example.com")
      browser.$("#reply").click
      browser.$("#sendbutton").click
      assertThat(browser.title()).isEqualTo("Feedback - TopicHub")
      browser.pageSource must contain("popcorn@example.com")
      browser.pageSource must contain("This field is required")
    }

    "submitted form generates an email" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      skipped
    }
  }
}
