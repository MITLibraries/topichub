import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._
import play.api.Play
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._
import models.User

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role, "identity")

  "Application" should {

    "send 404 on a bad request" in new WithApplication(FakeApplication(additionalConfiguration = inMemoryDatabase())){
      val bogus = route(FakeRequest(GET, "/boum")).get
      status(bogus) must equalTo(NOT_FOUND)
    }

    "render the index page" in new WithApplication(FakeApplication(additionalConfiguration = inMemoryDatabase())){
      val home = route(FakeRequest(GET, "/")).get

      status(home) must equalTo(OK)
      contentType(home) must beSome.which(_ == "text/html")
      contentAsString(home) must contain ("Add a description in conf/brand.conf")
    }

    "display a login screen" in new WithApplication(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val action = route(FakeRequest(GET, "/login")).get
      status(action) must equalTo(OK)
      contentType(action) must beSome.which(_ == "text/html")
      contentAsString(action) must contain (Play.configuration.getString("auth.login_text").get)
    }

    "display login screen when a non-logged in user asks for an analyst page" in new WithApplication(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val action = route(FakeRequest(GET, "/workbench")).get
      redirectLocation(action) must beSome.which(_ == "/login")
    }

    "display error screen when non analyst tries to access analyst protected page" in new WithApplication(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("schmuck")
      val action = route(FakeRequest(GET, "/workbench").withSession(("connected", user.identity))).get
      redirectLocation(action) must beNone
      contentAsString(action) must contain ("Reason: You are not authorized")
    }

    "display protected analyst page when analyst requests" in new WithApplication(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
      val user = create_user("analyst")
      println("user created ---" + user + "------")
      val action = route(FakeRequest(GET, "/workbench").withSession(("connected", user.identity))).get
      redirectLocation(action) must beNone
      contentAsString(action) must not contain (Play.configuration.getString("auth.login_text").get)
    }
  }
}
