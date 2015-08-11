import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ User }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class SysAdminPagesSpec extends Specification {

  "SysAdmin pages" should {

    def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                              "https://oidc.mit.edu/current_user")

    // GET /reindex
    "reindex" should {
      "as an unauthenticated User redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/reindex/topic")
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      "as an analyst redirects to Error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/reindex/topic")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      "as an admin reindexes" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("sysadmin")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/reindex/topic")
        browser.pageSource must contain("Reindexing topics")
      }
    }

    // GET /workbench
    "workbench" should {
      "as an unauthenticated User redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/workbench")
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      "as an analyst displays workbench" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/workbench")
        assertThat(browser.title()).isEqualTo("Workbench - TopicHub")
      }

      "as an admin redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("sysadmin")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/workbench")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }
    }

    // GET /purge
    "purge" should {
      "as an unauthenticated User redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/purge")
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      "as an analyst redirects to Error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/purge")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      "as an admin reindexes" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("sysadmin")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/purge")
        browser.pageSource must contain("too late to go back now")
      }
    }

    // GET /sandbox
    "sandbox" should {
      "as an unauthenticated User redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/sandbox")
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      "as an analyst displays form" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/sandbox")
        assertThat(browser.title()).isEqualTo("Sandbox - TopicHub")
        browser.pageSource must contain("""<form action="/testExpression" method="POST">""")
      }

      "as an admin redirects to Error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("sysadmin")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/sandbox")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }
    }

    // POST /testExpression
    "testExpression" should {
      "as an unauthenticated User redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val action = route(FakeRequest(POST, "/testExpression")).get
        redirectLocation(action) must beSome.which(_ == "/login")
      }

      "as an analyst displays form" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("analyst")
        val action = route(FakeRequest(POST, "/testExpression").
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("This field is required")
      }

      "as an admin redirects to Error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("sysadmin")
        val action = route(FakeRequest(POST, "/testExpression").
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }
    }

    // GET /model/create
    "contentModel" should {
      "as an unauthenticated User redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        browser.goTo("http://localhost:" + port + "/model/create")
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      "as an analyst redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("analyst")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/model/create")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      "as an admin display form" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("sysadmin")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/model/create")
        assertThat(browser.title()).isEqualTo("Create Model - TopicHub")
      }
    }

    // POST /cmodel
    "addContentModel" should {
      "as an unauthenticated User redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val action = route(FakeRequest(POST, "/cmodel")).get
        redirectLocation(action) must beSome.which(_ == "/login")
      }

      "as an analyst redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("analyst")
        val action = route(FakeRequest(POST, "/cmodel").
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }

      "as an admin is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("sysadmin")
        val action = route(FakeRequest(POST, "/cmodel").
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("This field is required")
      }
    }

    // POST /pubmodel
    "addPublisherModel" should {
      "as an unauthenticated User redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val action = route(FakeRequest(POST, "/pubmodel")).get
        redirectLocation(action) must beSome.which(_ == "/login")
      }

      "as an analyst redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("analyst")
        val action = route(FakeRequest(POST, "/pubmodel").
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }

      "as an admin is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("sysadmin")
        val action = route(FakeRequest(POST, "/pubmodel").
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("This field is required")
      }
    }

    // POST /submodel
    "addSubscriberModel" should {
      "as an unauthenticated User redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val action = route(FakeRequest(POST, "/submodel")).get
        redirectLocation(action) must beSome.which(_ == "/login")
      }

      "as an analyst redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("analyst")
        val action = route(FakeRequest(POST, "/submodel").
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }

      "as an admin is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("sysadmin")
        val action = route(FakeRequest(POST, "/submodel").
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("This field is required")
      }
    }
  }
}
