import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat

/**
 * add your integration spec here.
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
@RunWith(classOf[JUnitRunner])
class IntegrationSpec extends Specification {

  "Application" should {
    "work from within a browser" in new WithBrowser {
      browser.goTo("http://localhost:" + port)
      browser.pageSource must contain("Welcome to SCOAP")  
      assertThat(browser.title()).isEqualTo("SCOAP3 - TopicHub")
    }

    "top navigation should allow Workbench access" in new WithBrowser {
      browser.goTo("http://localhost:" + port)
      browser.$(".navbar-header a").getTexts().get(0) must equalTo("SCOAP3 TopicHub")
      browser.$("body > nav > div > div.navbar-header > button").click();
      browser.$("a[href*='workbench']").click();
      assertThat(browser.title()).isEqualTo("Workbench - TopicHub")
    }
  }

  "Workbench" should {
    "contains description of site section" in new WithBrowser {
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.pageSource).contains("Analytics Workbench")
      assertThat(browser.pageSource).contains("To perform the essential work of assigning topics")
    }

    "provides link to Model" in new WithBrowser {
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_models").getTexts.get(0)).isEqualTo("Models")
      browser.$("#sidenav_models").click();
      assertThat(browser.title()).isEqualTo("Create Model - TopicHub")
    }

    "provides link to Schemes" in new WithBrowser {
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_schemes").getTexts.get(0)).isEqualTo("Schemes")
      browser.$("#sidenav_schemes").click();
      assertThat(browser.title()).isEqualTo("Schemes - TopicHub")
    }

    "provides link to Content Types" in new WithBrowser {
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_ctypes").getTexts.get(0)).isEqualTo("Content Types")
      browser.$("#sidenav_ctypes").click();
      assertThat(browser.title()).isEqualTo("Content Types - TopicHub")
    }

    "provides link to Content Formats" in new WithBrowser {
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_cformats").getTexts.get(0)).isEqualTo("Content Formats")
      browser.$("#sidenav_cformats").click();
      assertThat(browser.title()).isEqualTo("Content Formats - TopicHub")
    }

    "provides link to Resource Maps" in new WithBrowser {
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_rmaps").getTexts.get(0)).isEqualTo("Resource Maps")
      browser.$("#sidenav_rmaps").click();
      assertThat(browser.title()).isEqualTo("Resource Maps - TopicHub")
    }

    "provides link to Publishers" in new WithBrowser {
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_publishers").getTexts.get(0)).isEqualTo("Publishers")
      browser.$("#sidenav_publishers").click();
      assertThat(browser.title()).isEqualTo("Publishers - TopicHub")
    }

    "provides link to Subscribers" in new WithBrowser {
      browser.goTo("http://localhost:" + port + "/workbench")
      assertThat(browser.$("#sidenav_subscribers").getTexts.get(0)).isEqualTo("Subscribers")
      browser.$("#sidenav_subscribers").click();
      assertThat(browser.title()).isEqualTo("Subscribers - TopicHub")
    }
  }
}
