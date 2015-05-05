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
class NavigationPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")

  "Top navigation" should {
    "as a non-authenticated User" should {
      "includes home" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port)
        browser.pageSource must contain("""<a class="navbar-brand" href="/">""")
      }

      "does not include dashboard" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub", "sub@example.com", "", "another_user")
        Subscriber.make(sub_user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port)
        browser.pageSource must not contain("""<a id="nav_sub_dashboard" href="/dashboard">""")
      }

      "does not include plans" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub", "sub@example.com", "", "another_user")
        Subscriber.make(sub_user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port)
        browser.pageSource must not contain(s"""<a id="nav_plans" href="/subscriber/">""")
      }

      "does not include workbench"  in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub", "sub@example.com", "", "another_user")
        Subscriber.make(sub_user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port)
        browser.pageSource must not contain("""<a id="nav_workbench" href="/workbench">""")
      }

      "includes search" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port)
        browser.pageSource must contain("""<a id="nav_search" href="/search">""")
      }

      "includes login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port)
        browser.pageSource must contain("""<a id="nav_login" href="/login">""")
      }

      "does not include logout" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port)
        browser.pageSource must not contain("""<a id="nav_login" href="/logout">""")
      }
    }

    "as a User with no affiliated Subscriber" should {
      "includes home" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must contain("""<a class="navbar-brand" href="/">""")
      }

      "does not include dashboard" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("")
        val sub_user = User.make("sub", "sub@example.com", "", "another_user")
        Subscriber.make(sub_user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must not contain("""<a id="nav_sub_dashboard" href="/dashboard">""")
      }

      "does not include plans" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("")
        val sub_user = User.make("sub", "sub@example.com", "", "another_user")
        Subscriber.make(sub_user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must not contain(s"""<a id="nav_plans" href="/subscriber/">""")
      }

      "does not include workbench" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("")
        val sub_user = User.make("sub", "sub@example.com", "", "another_user")
        Subscriber.make(sub_user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must not contain("""<a id="nav_workbench" href="/workbench">""")
      }

      "includes search" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must contain("""<a id="nav_search" href="/search">""")
      }

      "does not includes login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must not contain("""<a id="nav_login" href="/login">""")
      }

      "includes logout" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must contain("""<a id="nav_login" href="/logout">""")
      }

      "logout ends session and flashes message" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/logout")
        browser.pageSource must contain("You are now signed out.")
        browser.pageSource must contain("""<a id="nav_login" href="/login">""")
        browser.pageSource must not contain("""<a id="nav_login" href="/logout">""")
      }
    }

    "as a User with an affiliated Subscriber" should {
      "includes dashboard with no counter if nothing to review" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.make(create_user("").id, "Sub Name", "cat", "contact",
                          Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must contain("""<a id="nav_sub_dashboard" href="/dashboard">""")
      }

      "includes dashboard with counter if something to review" in {
        skipped(": I'm too lazy to write this now")
      }

      "includes plans" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Subscriber.make(create_user("").id, "Sub Name", "cat", "contact",
                          Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must contain(s"""<a id="nav_plans" href="/subscriber/${s.id}/edit">""")
      }

      "does not include workbench" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val s = Subscriber.make(create_user("").id, "Sub Name", "cat", "contact",
                                Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must not contain("""<a id="nav_workbench" href="/workbench">""")
      }
    }

    "as a User with the Analyst role" should {
      "includes workbench" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.make(create_user("analyst").id, "Sub Name", "cat", "contact",
                          Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must contain("""<a id="nav_workbench" href="/workbench">""")
      }
    }

    "as a User with the Admin role" should {
      "does not include workbench" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        Subscriber.make(create_user("sysadmin").id, "Sub Name", "cat", "contact",
                          Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.pageSource must not contain("""<a id="nav_workbench" href="/workbench">""")
      }
    }
  }
}
