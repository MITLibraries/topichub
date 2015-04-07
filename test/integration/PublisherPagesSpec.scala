import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Publisher, User, Subscriber }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class PublisherPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role, "identity")
  def make_subscriber(userid: Int) = Subscriber.make(userid, "Sub Name", "cat", "contact", Some("link"), Some("logo"))

  "Publisher pages" should {
    "index" should {
      "display information about publishers" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/publishers")
        assertThat(browser.title()).isEqualTo("Publishers - TopicHub")
        browser.pageSource must contain("Contribute your content to TopicHub")
      }

      "display sign up link" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/publishers")
        browser.pageSource must contain("/publishers/create")
      }

      "display links to browse exisiting publisher categories" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        browser.goTo("http://localhost:" + port + "/publishers")
        browser.pageSource must contain("/publishers/browse?filter=category&amp;value=pubcat")
      }
    }

    "browse categories" should {
      "display links to publishers" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))

        browser.goTo("http://localhost:" + port + "/publishers")
        browser.$("a[href*='pubcat']").click();
        assertThat(browser.title()).isEqualTo("Publisher Browse - TopicHub")
      }
    }

    "view publisher" should {
      "display publishers page" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some("http://www.example.com"), Some(""))
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id)
        assertThat(browser.title()).isEqualTo("Publisher - TopicHub")
        browser.pageSource must contain("No articles deposited")
        browser.$("a[href*='http://www.example.com']").getText() must equalTo("Site »")
      }

      "with no publisher link displays no link" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", None, Some(""))
        browser.goTo("http://localhost:" + port + "/publisher/" + pub.id)
        browser.pageSource must not contain("Site »")
      }

      "display links to publisher collections" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        skipped
      }

      "edit link" should {
        "displays to user associated with publisher" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
          val user = create_user("norole")
          val sub = make_subscriber(user.id)
          val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some("http://www.example.com"), Some(""))
          browser.goTo("http://localhost:" + port + "/publisher/" + pub.id)
          browser.pageSource must contain("/publisher/1/edit")
        }

        "not display to non logged in user" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
          skipped(": need to be able to create a publisher without signing in user during test")
        }

        "not display to logged in user not associated with publisher" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
          val user = create_user("norole")
          val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
          val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some("http://www.example.com"), Some(""))
          browser.goTo("http://localhost:" + port + "/publisher/" + pub.id)
          browser.pageSource must not contain("/publisher/1/edit")
        }
      }
    }

    "create publisher" should {
      "redirect to login if not signed in" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/publishers")
        browser.$("a[href*='/publishers/create']").click()
        browser.pageSource must contain("Log in with your MIT ID")
        assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      }

      "display create publisher form if signed in" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        browser.goTo("http://localhost:" + port + "/publishers")
        browser.$("a[href*='/publishers/create']").click()
        assertThat(browser.title()).isEqualTo("Create Publisher - TopicHub")
      }

      "creates publisher on submit" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        browser.goTo("http://localhost:" + port + "/publishers/create")
        browser.$("#tag").text("New Publisher Tag")
        browser.$("#name").text("New Publisher Name")
        browser.$("#description").text("New Publisher Description")
        browser.$("#submit").click
        assertThat(browser.title()).isEqualTo("Edit Publisher - TopicHub")
      }
    }

    "edit publisher" should {
      "redirects to login if not signed in" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        skipped(": need to be able to create a publisher without signing in user during test")
      }

      "redirects to trouble if user does not own publisher" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub_user = User.make("pub", "pub@example.com", "some roles", "another_identity")
        val pub = Publisher.make(pub_user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some("http://www.example.com"), Some(""))

        browser.goTo("http://localhost:" + port + "/publisher/1/edit")
        browser.pageSource must contain("Reason: You are not authorized")
      }

      "displays edit page if user owns publisher" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("norole")
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some("http://www.example.com"), Some(""))

        browser.goTo("http://localhost:" + port + "/publisher/1/edit")
        assertThat(browser.title()).isEqualTo("Edit Publisher - TopicHub")
        browser.pageSource must contain("/publisher/" + pub.id + "/create")
        browser.pageSource must contain("/publisher/" + pub.id + "/createH")
      }
    }
  }
}
