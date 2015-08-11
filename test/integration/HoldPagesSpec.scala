import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Collection, ContentType, Hold, Item, Publisher, ResourceMap,
                Scheme, User, Subscriber, Subscription, Topic }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class HoldPagesSpec extends Specification {

  "Hold pages" should {
    "as an unauthenticated User" should {

      // GET /holds/browse?id=sub.id&page=x
      "browse redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val subscr = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        val pub = Publisher.make(sub_user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        Hold.create(sub.id, subscr.id, item.id)
        browser.goTo("http://localhost:" + port + "/holds/browse?id=" + sub.id)
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      // GET /hold/:id/resolve?accept=Boolean
      "resolve redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val subscr = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        val pub = Publisher.make(sub_user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        var h = Hold.make(sub.id, subscr.id, item.id)
        Hold.held(item.id, sub.id) must equalTo(true)
        browser.goTo("http://localhost:" + port + "/hold/" + h.id + "/resolve?accept=false")
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
        Hold.held(item.id, sub.id) must equalTo(true)
        pending("see https://github.com/MITLibraries/scoap3hub/issues/170")
      }
    }

    "as a User not accociated with Subscriber" should {

      // GET /holds/browse?id=sub.id&page=x
      "browse redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val user_sub = Subscriber.make(user.id, "User Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val subscr = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        Hold.create(sub.id, subscr.id, item.id)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/holds/browse?id=" + sub.id)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
      }

      // GET /hold/:id/resolve?accept=Boolean
      "resolve redirects to error" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("user_name", "user@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val user_sub = Subscriber.make(user.id, "User Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val sub_user = User.make("sub_name", "sub@example.com", "role", "sub_identity")
        val sub = Subscriber.make(sub_user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val subscr = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        var h = Hold.make(sub.id, subscr.id, item.id)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        Hold.held(item.id, sub.id) must equalTo(true)
        browser.goTo("http://localhost:" + port + "/hold/" + h.id + "/resolve?accept=false")
        Hold.held(item.id, sub.id) must equalTo(true)
        assertThat(browser.title()).isEqualTo("Error - TopicHub")
        browser.pageSource must contain("You are not authorized")
        pending("see https://github.com/MITLibraries/scoap3hub/issues/170")
      }
    }

    "as a User associated with Subscriber" should {

      // GET /holds/browse?id=sub.id&page=x
      "browse is allowed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("sub_name", "sub@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val subscr = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        Hold.create(sub.id, subscr.id, item.id)
        browser.goTo("http://localhost:" + port + "/holds/browse?id=" + sub.id)
        assertThat(browser.title()).isEqualTo("Holds Browse - TopicHub")
      }

      // GET /hold/:id/resolve?accept=Boolean
      "resolve with true removes hold" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("sub_name", "sub@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val subscr = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        var h = Hold.make(sub.id, subscr.id, item.id)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        Hold.held(item.id, sub.id) must equalTo(true)
        browser.goTo("http://localhost:" + port + "/hold/" + h.id + "/resolve?accept=true")
        Hold.held(item.id, sub.id) must equalTo(false)
      }.pendingUntilFixed("see https://github.com/MITLibraries/scoap3hub/issues/170")

      // GET /hold/:id/resolve?accept=Boolean
      "resolve with false removes hold" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("sub_name", "sub@example.com", "role",
                             "https://oidc.mit.edu/current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val scheme = Scheme.make("tag", "gentype", "cat", "desc", Some("link"), Some("logo"))
        val t = Topic.make(scheme.id, "tag0", "name0")
        val subscr = Subscription.make(sub.id, t.id, "deliver", sub.created, sub.created)
        val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat",
                                 "pubstatus", Some(""), Some(""))
        val ct = ContentType.make("cttag", "ctlabel", "ctdesc", Some(""))
        val rmap = ResourceMap.make("rmaptag", "rmapdesc", Some(""))
        var col = Collection.make(pub.id, ct.id, rmap.id, "coll", "desc", "open")
        val item = Item.make(col.id, ct.id, "location", "scoap:abc:123")
        var h = Hold.make(sub.id, subscr.id, item.id)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        Hold.held(item.id, sub.id) must equalTo(true)
        browser.goTo("http://localhost:" + port + "/hold/" + h.id + "/resolve?accept=false")
        Hold.held(item.id, sub.id) must equalTo(false)
      }.pendingUntilFixed("see https://github.com/MITLibraries/scoap3hub/issues/170")
    }
  }
}
