import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._
import org.specs2.mock._
import java.util.Date

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import models.{ Subscriber, User }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class SubscriberUserPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")
  def make_subscriber(userid: Int) =
    Subscriber.make(userid, "Sub Name", "cat","contact", Some("link"), Some("logo"))

  "new Subscriber form should" should {
    "prompt for login when not signed in" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/subscribers/create")
      assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      browser.pageSource must contain("Log in with your MIT ID")
    }

    "displays a link to join when there are Subscribers" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
      val sub = make_subscriber(sub_user.id)
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      assertThat(browser.title()).isEqualTo("Create Subscriber - TopicHub")
      browser.pageSource must contain("request to join")
    }

    "does not display a link to join when there are no Subscribers" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      assertThat(browser.title()).isEqualTo("Create Subscriber - TopicHub")
      browser.pageSource must not contain("request to join")
    }
  }

  "join subscriber form" should {
    "prompt for login when not signed in" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/subscribers/join")
      assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      browser.pageSource must contain("Log in with your MIT ID")
    }

    "allows User to request to join Subscriber when unjoined Subscribers exist" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
      val sub = make_subscriber(sub_user.id)
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/subscribers/join")
      assertThat(browser.title()).isEqualTo("Join Subscriber - TopicHub")
      browser.$("#submit").click
      browser.pageSource must contain("Your request to join the Subscriber Group has been submitted.")
      sub.userList(approved = false).contains(user) must equalTo(true)
    }

    "does not show Subscribers to join the User is already a member of" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
      val sub = make_subscriber(sub_user.id)
      sub.linkUser(user.id, approved=true)
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/subscribers/join")
      browser.pageSource must contain("There are no Subscibers to join.")
      browser.pageSource must contain("Create a New Subscriber?")
    }

    "displays message if there are no Subscribers to join" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/subscribers/join")
      assertThat(browser.title()).isEqualTo("Join Subscriber - TopicHub")
      browser.pageSource must contain("There are no Subscibers to join.")
      browser.pageSource must contain("""<a href="/subscribers/create""")
    }
  }

  "subscriber user list page" should {
    "prompt for login when not signed in" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
      val sub = make_subscriber(sub_user.id)

      browser.goTo("http://localhost:" + port + "/subscriber/1/users")
      assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
      browser.pageSource must contain("Log in with your MIT ID")
    }

    "deny access for users not in the Subscriber group" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
      val sub = make_subscriber(sub_user.id)
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/subscriber/1/users")
      assertThat(browser.title()).isEqualTo("Error - TopicHub")
      browser.pageSource must contain("You are not authorized")
    }

    "for group admin users" should {
      "shows approved and pending user lists" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub = make_subscriber(user.id)

        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(another_user.id)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/subscriber/1/users")
        assertThat(browser.title()).isEqualTo("Subscriber Users - TopicHub")
        browser.pageSource must contain("Current Users")
        browser.pageSource must contain(user.email)

        // shows non-approved users to non-admins
        browser.pageSource must contain("Pending Users")
        browser.pageSource must contain(another_user.email)
      }

      "can approve pending users" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub = make_subscriber(user.id)
        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(another_user.id)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/subscriber/1/users")
        assertThat(browser.title()).isEqualTo("Subscriber Users - TopicHub")
        browser.pageSource must contain(another_user.email)
        sub.userList(approved = true) must not contain(another_user)
        browser.$("#approve_" + another_user.id).click
        sub.userList(approved = true) must contain(another_user)
        browser.pageSource must contain(another_user.email)
      }

      "can reject pending users" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub = make_subscriber(user.id)
        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(another_user.id)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/subscriber/1/users")
        assertThat(browser.title()).isEqualTo("Subscriber Users - TopicHub")
        browser.pageSource must contain(another_user.email)
        sub.userList(approved = true) must not contain(another_user)
        sub.userList(approved = false) must contain(another_user)

        browser.$("#deny_" + another_user.id).click
        sub.userList(approved = true) must not contain(another_user)
        sub.userList(approved = false) must not contain(another_user)
        browser.pageSource must not contain(another_user.email)
      }

      "can remove users from group" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub = make_subscriber(user.id)
        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(another_user.id, approved = true)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/subscriber/1/users")
        assertThat(browser.title()).isEqualTo("Subscriber Users - TopicHub")
        sub.userList(approved = true) must contain(another_user)

        browser.$("#remove_" + another_user.id).click
        sub.userList(approved = true) must not contain(another_user)
        sub.userList(approved = false) must not contain(another_user)
        browser.pageSource must not contain(another_user.email)
      }

      "can make a user an admin for group" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub = make_subscriber(user.id)
        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(another_user.id, approved = true)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/subscriber/1/users")
        assertThat(browser.title()).isEqualTo("Subscriber Users - TopicHub")
        sub.adminList must not contain(another_user)

        browser.$("#toggleadmin_" + another_user.id).click
        sub.adminList must contain(another_user)
      }

      "can remove admin from a user for group" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub = make_subscriber(user.id)
        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(another_user.id, approved = true, admin = true)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/subscriber/1/users")
        assertThat(browser.title()).isEqualTo("Subscriber Users - TopicHub")
        sub.adminList must contain(another_user)

        browser.$("#toggleadmin_" + another_user.id).click
        sub.adminList must not contain(another_user)
      }

      "displays message if not pending users for the group" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub = make_subscriber(user.id)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/subscriber/1/users")
        assertThat(browser.title()).isEqualTo("Subscriber Users - TopicHub")
        browser.pageSource must contain("There are no users waiting for approval")
      }
    }

    "for non-admin group members" should {
      "shows approved user list" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
        val sub = make_subscriber(sub_user.id)
        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(user.id, approved=true)
        sub.linkUser(another_user.id)

        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/subscriber/1/users")
        assertThat(browser.title()).isEqualTo("Subscriber Users - TopicHub")
        browser.pageSource must contain("Current Users")
        browser.pageSource must contain(user.email)

        // does not show non-approved users to non-admins
        browser.pageSource must not contain("Pending Users")
        browser.pageSource must not contain(another_user.email)
      }

      "cannot approve pending members" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
        val sub = make_subscriber(sub_user.id)
        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(user.id, approved=true)
        sub.linkUser(another_user.id)
        val action = route(FakeRequest(
                            GET, s"/subscriber/${sub.id}/resolve/${another_user.id}?res=approve").
                            withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }

      "cannot reject pending members" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
        val sub = make_subscriber(sub_user.id)
        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(user.id, approved=true)
        sub.linkUser(another_user.id)
        val action = route(FakeRequest(
                            GET, s"/subscriber/${sub.id}/resolve/${another_user.id}?res=reject").
                            withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }

      "cannot make members admins" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
        val sub = make_subscriber(sub_user.id)
        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(user.id, approved=true)
        sub.linkUser(another_user.id, approved=true)
        val action = route(FakeRequest(
                            GET, s"/subscriber/${sub.id}/toggleadmin/${another_user.id}").
                            withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }

      "cannot remove admin from member" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
        val sub = make_subscriber(sub_user.id)
        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(user.id, approved=true)
        sub.linkUser(another_user.id, approved=true, admin=true)
        val action = route(FakeRequest(
                            GET, s"/subscriber/${sub.id}/toggleadmin/${another_user.id}").
                            withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }

      "cannot remove member" in new WithBrowser(
          app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("schmuck")
        val sub_user = User.make("sub", "sub@example.com", "role", "sub_identity")
        val sub = make_subscriber(sub_user.id)
        val another_user = User.make("another_user", "another@example.com", "", "another_identity")
        sub.linkUser(user.id, approved=true)
        sub.linkUser(another_user.id, approved=true)
        val action = route(FakeRequest(
                            GET, s"/subscriber/${sub.id}/remove/${another_user.id}").
                            withSession(("connected", user.identity))).get
        redirectLocation(action) must beNone
        contentAsString(action) must contain ("Reason: You are not authorized")
      }
    }
  }

  "Session Subscriber" should {
    "points to non-existent Subscriber is treated like not set" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      val sub = make_subscriber(user.id)
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      sub.delete
      browser.goTo("http://localhost:" + port)
      browser.pageSource must not contain("""<a id="nav_sub_dashboard" href="/dashboard">""")
    }

    "does not include User as member is treated like not set" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      val sub = make_subscriber(user.id)
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      sub.unlinkUser(user.id)
      browser.goTo("http://localhost:" + port)
      browser.pageSource must not contain("""<a id="nav_sub_dashboard" href="/dashboard">""")
    }

    "is valid and associated with current user" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      val sub = make_subscriber(user.id)
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port)
      browser.pageSource must contain("""<a id="nav_sub_dashboard" href="/dashboard">""")
    }

    "is set for Current User when creating Subscriber" in  new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.pageSource must not contain("""<a id="nav_sub_dashboard" href="/dashboard">""")

      browser.$("#name").text("Some Subscriber Name")
      browser.$("#contact").text("sub@example.com")
      browser.$("#link").text("http://example.com")
      browser.$("#logo").text("http://example.com/logo.png")
      browser.$("#submit").click

      browser.pageSource must contain("""<a id="nav_sub_dashboard" href="/dashboard">""")
    }
  }
}
