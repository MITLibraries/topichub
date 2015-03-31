import org.specs2.mutable._
import org.specs2.runner._
import org.junit.runner._

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
@RunWith(classOf[JUnitRunner])
class ApplicationSpec extends Specification {

  "Application" should {

    "send 404 on a bad request" in new WithApplication{
      route(FakeRequest(GET, "/boum")) must beNone
    }

    "render the index page" in new WithApplication{
      val home = route(FakeRequest(GET, "/")).get

      status(home) must equalTo(OK)
      contentType(home) must beSome.which(_ == "text/html")
      contentAsString(home) must contain ("Welcome to SCOAP")
    }

    "display a login screen" in new WithBrowser {
      val action = route(FakeRequest(GET, "/login")).get
      status(action) must equalTo(OK)
      contentType(action) must beSome.which(_ == "text/html")
      contentAsString(action) must contain ("Log in with MITID")
    }

//     "parse returned user json" in new WithBrowser {
//       val action = route(
//         FakeRequest(GET, "/_oauth-success")
//           .withHeaders(CONTENT_TYPE -> "application/json")
//           .withSession(("oauth-token", "asdffdsa"))
//           .withBody("""
// {"sub":"asdffdsa","name":"Some M User","preferred_username":"suser","given_name":"Some","family_name":"User","middle_name":"M","email":"suser@example.edu","email_verified":true}
//             """)
//         ).get
//       status(action) must equalTo(OK)
//       contentType(action) must beSome.which(_ == "application/json")
//       contentAsString(action) must contain ("Log in with MITID")
//     }
  }
}
