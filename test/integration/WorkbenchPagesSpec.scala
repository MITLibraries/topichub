import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import models.User
import play.api.Application
import play.api.Play
import play.api.Play.current

/**
 * add your integration spec here.
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
@RunWith(classOf[JUnitRunner])
class WorkbenchPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")

  "Application" should {
    "work from within a browser" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port)
      browser.pageSource must contain("Welcome to SCOAP")
      assertThat(browser.title()).isEqualTo("SCOAP3 - TopicHub")
    }

    "top navigation should allow Workbench access to analyst" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port)
      browser.$(".navbar-header a").getTexts().get(0) must equalTo("SCOAP3 TopicHub")
      browser.$("body > nav > div > div.navbar-header > button").click();
      browser.$("a[href*='workbench']").click();
      assertThat(browser.title()).isEqualTo("Workbench - TopicHub")
    }

    "deny Workbench access to non analyst" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.title()).isEqualTo("Error - TopicHub")
      browser.pageSource must contain("You are not authorized")
    }

    "deny Workbench access to not logged in user" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.title()).isEqualTo("Login to SCOAP3 - TopicHub")
    }
  }

  "Workbench for SysAdmins" should {
    "provide link to Model" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("sysadmin, analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      browser.pageSource must contain("""<a id="sidenav_models" href="/model/create""")
      browser.$("#sidenav_models").click
      assertThat(browser.title()).isEqualTo("Create Model - TopicHub")
    }

    "provides link to items with no topics" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("sysadmin, analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      browser.pageSource must contain("""<a id="sidenav_notopic_items" href="/items/missingtopics""")
      browser.$("#sidenav_notopic_items").click
      assertThat(browser.title()).isEqualTo("Items missing Topics - TopicHub")
    }
  }

  "Workbench for analysts" should {
    "contains description of site section" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.pageSource).contains("Analytics Workbench")
      assertThat(browser.pageSource).contains("To perform the essential work of assigning topics")
    }

    "does not provide link to Model" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      browser.pageSource must not contain("""<a id="sidenav_models" href="/model/create""")
      browser.goTo("http://localhost:" + port + "/model/create")
      assertThat(browser.title()).isEqualTo("Error - TopicHub")
      browser.pageSource must contain("You are not authorized")
    }

    "does not provide link to items with no topics" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      browser.pageSource must not contain("""<a id="sidenav_notopic_items" href="/items/missingtopics""")
    }

    "provides link to Schemes" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_schemes").getTexts.get(0)).isEqualTo("Schemes")
      browser.$("#sidenav_schemes").click();
      assertThat(browser.title()).isEqualTo("Schemes - TopicHub")
    }

    "provides link to Content Types" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_ctypes").getTexts.get(0)).isEqualTo("Content Types")
      browser.$("#sidenav_ctypes").click();
      assertThat(browser.title()).isEqualTo("Content Types - TopicHub")
    }

    "provides link to Content Formats" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_cformats").getTexts.get(0)).isEqualTo("Content Formats")
      browser.$("#sidenav_cformats").click();
      assertThat(browser.title()).isEqualTo("Content Formats - TopicHub")
    }

    "provides link to Resource Maps" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_rmaps").getTexts.get(0)).isEqualTo("Resource Maps")
      browser.$("#sidenav_rmaps").click();
      assertThat(browser.title()).isEqualTo("Resource Maps - TopicHub")
    }

    "provides link to Publishers" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_publishers").getTexts.get(0)).isEqualTo("Publishers")
      browser.$("#sidenav_publishers").click();
      assertThat(browser.title()).isEqualTo("Publishers - TopicHub")
    }

    "provides link to Subscribers" in new WithBrowser(app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("analyst")
      browser.goTo("http://localhost:" + port + "/login")
      browser.$("#openid").click
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_subscribers").getTexts.get(0)).isEqualTo("Subscribers")
      browser.$("#sidenav_subscribers").click();
      assertThat(browser.title()).isEqualTo("Subscribers - TopicHub")
    }
  }
}
