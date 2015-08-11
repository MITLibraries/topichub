import org.specs2.mutable._
import org.specs2.runner._

import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Agent, Channel, Collection, ContentType, Plan, Publisher, ResourceMap, Scheme,
                Subscriber, Subscription, Topic, TopicPick, User }

/**
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class TopicPagesSpec extends Specification {

  def create_user(role: String) = User.make("bob", "bob@example.com", role,
                                            "https://oidc.mit.edu/current_user")
  def topic_factory(count: Int) {
    val ct = ContentType.make("tag", "label", "desc", Some("logo"))
    val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
    val user = User.make("pubuser", "pubuser@example.com", "", "pub_identity")
    val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                             Some(""), Some(""))
    val s = Scheme.make("tag", "topic", "cat", "SomeScheme", Some("link"), Some("logo"))
    val col = Collection.make(pub.id, ct.id, rm.id, "coll1_tag", "coll1 desc", "open")
    1 to count foreach { t => Topic.make(s.id, "tag0" + t, "name0" + t) }
  }

  "Topic pages" should {
    "as an unauthenticated User" should {

      // GET /topics
      "topics index works" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        topic_factory(11)
        browser.goTo("http://localhost:" + port + "/topics")
        assertThat(browser.title()).isEqualTo("Topics - TopicHub")
        browser.pageSource must contain("Understanding Topics")
        browser.pageSource must contain("tag: SomeScheme")
        browser.pageSource must contain("(11 topics)")
      }

      // GET /topic/:id
      "accessing an topic works" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        topic_factory(1)
        browser.goTo("http://localhost:" + port + "/topic/1")
        assertThat(browser.title()).isEqualTo("Topic - TopicHub")
        browser.pageSource must contain("Recent Articles")
      }

      // GET /topic/:id/subscribe
      "subscribing to a topic redirects to login" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        topic_factory(1)
        browser.goTo("http://localhost:" + port + "/topic/1/subscribe")
        assertThat(browser.title()).isEqualTo("Login to TopicHub")
      }

      // GET /topics/browse
      "browsing topics works" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        topic_factory(11)
        Topic.all.size must equalTo(11)
        browser.goTo("http://localhost:" + port + "/topics/browse?id=1")
        assertThat(browser.title()).isEqualTo("Topic Browse - TopicHub")
        browser.pageSource must contain("Viewing 1 - 10 of 11")
        browser.$("#next_page").click
        browser.pageSource must contain("Viewing 11 - 11 of 11")
        browser.$("#prev_page").click
        browser.pageSource must contain("Viewing 1 - 10 of 11")
      }

      // GET /topic/:sid/validate
      "validating topics works" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        skipped("should this work for non authenticated users? I'm not sure")
      }
    }

    "as a signed in user with no subscriber affiliated" should {
      // GET /topic/:id/subscribe
      "subscribing to a topic redirects to Subscribers page" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        create_user("current_user")
        topic_factory(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/topic/1/subscribe")
        assertThat(browser.title()).isEqualTo("Subscribers - TopicHub")
        browser.pageSource must contain("Sign Up »")
      }
    }

    "as a signed in user with a subscriber affiliated" should {
      // GET /topic/:id
      "accessing a topic with no active subscription and a plan displays subscribe" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid", "password",
                                "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                             "review", "subscribe", "review")
        topic_factory(1)
        val s = Scheme.findByTag("tag").get
        plan.addScheme(s)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/topic/1")
        assertThat(browser.title()).isEqualTo("Topic - TopicHub")
        browser.pageSource must not contain("Cancel subscription to this topic")
        browser.pageSource must contain("Subscribe to this topic")
        browser.pageSource must not contain("Forget this topic")
      }

      "accessing a topic with no active subscription and no plan does not display subscribe" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("current_user")
        Subscriber.make(user.id, "Sub Name", "cat", "contact", Some("link"), Some("logo"))
        topic_factory(1)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/topic/1")
        assertThat(browser.title()).isEqualTo("Topic - TopicHub")
        browser.pageSource must not contain("Cancel subscription to this topic")
        browser.pageSource must not contain("Subscribe to this topic")
        browser.pageSource must not contain("Forget this topic")
      }

      // GET /topic/:id
      "accessing a topic with no active subscription but with pick displays subscribe and meh" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid", "password",
                                "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                             "review", "subscribe", "review")
        topic_factory(1)
        val s = Scheme.findByTag("tag").get
        plan.addScheme(s)
        val agent = Agent.make("tag", "label", "description", "code", "params", Some("icon"))
        TopicPick.make(sub.id, 1, agent.id)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/topic/1")
        browser.pageSource must not contain("Cancel subscription to this topic")
        browser.pageSource must contain("Subscribe to this topic")
        browser.pageSource must contain("Forget this topic")
      }

      // GET /topic/:id
      "accessing a topic with active subscription displays unsubscribe" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid", "password",
                                "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                             "review", "subscribe", "review")
        topic_factory(1)
        val s = Scheme.findByTag("tag").get
        plan.addScheme(s)
        Subscription.make(sub.id, 1, "deliver", sub.created, sub.created)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/topic/1")
        browser.pageSource must contain("Cancel subscription to this topic")
        browser.pageSource must not contain("Subscribe to this topic")
        browser.pageSource must not contain("Forget this topic")
      }

      // GET /topic/:id/subscribe
      "subscribing to a topic adds subscription if not subscribed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid", "password",
                                "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                             "review", "subscribe", "review")
        topic_factory(1)
        val s = Scheme.findByTag("tag").get
        plan.addScheme(s)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/topic/1/subscribe?cancel=false")
        browser.pageSource must contain("Cancel subscription to this topic")
        browser.pageSource must not contain("Subscribe to this topic")
        browser.pageSource must not contain("Forget this topic")
      }

      // GET /topic/:id/subscribe
      "subscribing to a topic removes subscription if subscribed" in new WithBrowser(
        app = FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = create_user("current_user")
        val sub = Subscriber.make(user.id, "Sub Name", "cat", "contact",
                                  Some("link"), Some("logo"))
        val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid", "password",
                                "http://example.com")
        val plan = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                             "review", "subscribe", "review")
        topic_factory(1)
        val s = Scheme.findByTag("tag").get
        plan.addScheme(s)
        Subscription.make(sub.id, 1, "deliver", sub.created, sub.created)
        browser.goTo("http://localhost:" + port + "/login")
        browser.$("#openid").click
        browser.goTo("http://localhost:" + port + "/topic/1")
        browser.pageSource must contain("Cancel subscription to this topic")
        browser.pageSource must not contain("Subscribe to this topic")
        browser.pageSource must not contain("Forget this topic")
        browser.goTo("http://localhost:" + port + "/topic/1/subscribe?cancel=true")
        browser.pageSource must not contain("Cancel subscription to this topic")
        browser.pageSource must contain("Subscribe to this topic")
        browser.pageSource must not contain("Forget this topic")
      }
    }
  }
}
