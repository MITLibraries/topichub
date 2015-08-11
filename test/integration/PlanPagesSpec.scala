import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Channel, Plan, Scheme, User, Subscriber }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class PlanPagesSpec extends Specification {

  "Plan pages" should {
    "as an unauthenticated User" should {

      // GET /plan/:id
      "show redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")

        browser.goTo("http://localhost:" + port + "/plan/" + p.id)
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      // GET /plans/:sid/create
      "new form redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/plans/" + sub.id + "/create")
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      // POST /plans/:sid
      "posting to create redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/plans/" + sub.id)).get
        redirectLocation(action) must beSome.which(_ == "/login")
      }

      // DELETE /plan/:id
      "delete redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        val action = route(FakeRequest(DELETE, "/plan/" + p.id)).get
        redirectLocation(action) must beSome.which(_ == "/login")
        Plan.findById(p.id).get must equalTo(p)
      }

      // POST /plan/:id
      "add scheme redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        val action = route(FakeRequest(POST, "/plan/" + p.id)).get
        redirectLocation(action) must beSome.which(_ == "/login")
        p.schemes.size must equalTo(0)
      }

      // GET /plan/:id/remove/:sid
      "remove scheme redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        p.addScheme(scheme)
        p.schemes.size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/plan/" + p.id + "/remove/" + scheme.id)
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
        p.schemes.size must equalTo(1)
      }
    }

    "as a User with no Associated Subscriber" should {

      // GET /plan/:id
      "show redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/plan/" + p.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /plans/:sid/create
      "new form redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/plans/" + sub.id + "/create")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // POST /plans/:sid
      "posting to create redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/plans/" + sub.id).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }

      // DELETE /plan/:id
      "delete redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        val action = route(FakeRequest(DELETE, "/plan/" + p.id).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        Plan.findById(p.id).get must equalTo(p)
      }

      // POST /plan/:id
      "add scheme redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/plan/" + p.id +"?scheme_id=" + scheme.id).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        p.schemes.size must equalTo(0)
      }

      // GET /plan/:id/remove/:sid
      "remove scheme redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        p.addScheme(scheme)
        p.schemes.size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/plan/" + p.id + "/remove/" + scheme.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
        p.schemes.size must equalTo(1)
      }
    }

    "as a User not associated with correct Subscriber" should {
      // GET /plan/:id
      "show redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val user_sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/plan/" + p.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /plans/:sid/create
      "new form redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val user_sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/plans/" + sub.id + "/create")
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // POST /plans/:sid
      "posting to create redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val user_sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/plans/" + sub.id).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }

      // DELETE /plan/:id
      "delete redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val user_sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        val action = route(FakeRequest(DELETE, "/plan/" + p.id).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        Plan.findById(p.id).get must equalTo(p)
      }

      // POST /plan/:id
      "add scheme redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val user_sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val action = route(FakeRequest(POST, "/plan/" + p.id +"?scheme_id=" + scheme.id).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
        p.schemes.size must equalTo(0)
      }

      // GET /plan/:id/remove/:sid
      "remove scheme redirects to trouble" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val user_sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        p.addScheme(scheme)
        p.schemes.size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/plan/" + p.id + "/remove/" + scheme.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
        p.schemes.size must equalTo(1)
      }
    }

    "as a User associated with correct Subscriber" should {

      // GET /plan/:id
      "show is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/plan/" + p.id)
        assertThat(browser.title()).isEqualTo("Subscriber Plan - TopicHub")
      }

      // GET /plans/:sid/create
      "new form is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/plans/" + sub.id + "/create")
        assertThat(browser.title()).isEqualTo("New Action Plan - TopicHub")
      }

      // POST /plans/:sid
      "posting to create is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/plans/" + sub.id + "/create")

        sub.plans.size must equalTo(0)
        // without required fields
        browser.$("#submit").click
        browser.pageSource must contain("This field is required")
        sub.plans.size must equalTo(0)

        // with required fields
        browser.$("#name").text("New Publisher Tag")
        browser.$("#description").text("New Publisher Name")
        browser.$("#submit").click
        browser.pageSource must not contain("This field is required")
        assertThat(browser.title()).isEqualTo("Subscriber - TopicHub")
        sub.plans.size must equalTo(1)
      }

      // DELETE /plan/:id
      "delete removes plan" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        sub.plans.size must equalTo(1)
        val action = route(FakeRequest(DELETE, "/plan/" + p.id).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beSome.which(_ == "/subscriber/" + sub.id)
        Plan.findById(p.id) must equalTo(None)
        sub.plans.size must equalTo(0)
      }

      // POST /plan/:id
      "add scheme links a scheme to the plan" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        p.schemes.size must equalTo(0)
        val action = route(FakeRequest(POST, "/plan/" + p.id +"?scheme_id=" + scheme.id).
                           withSession(("connected", user.identity))).get
        redirectLocation(action) must beSome.which(_ == "/subscriber/" + sub.id + "/edit")
        p.schemes.size must equalTo(1)
      }

      // GET /plan/:id/remove/:sid
      "remove scheme unlinks a scheme from the plan" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                                "password", "http://example.com")
        val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                          "review", "subscribe", "review")
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        p.addScheme(scheme)
        p.schemes.size must equalTo(1)
        browser.goTo("http://localhost:" + port + "/plan/" + p.id + "/remove/" + scheme.id)
        assertThat(browser.title()).isEqualTo("Edit Subscriber - TopicHub")
        p.schemes.size must equalTo(0)
      }
    }
  }
}
