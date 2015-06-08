import org.specs2.mutable._
import org.specs2.runner._
import play.api.test._
import play.api.test.Helpers._
import org.fest.assertions.Assertions.assertThat
import play.api.Application
import play.api.Play
import play.api.Play.current
import models.{ Channel, Collection, ContentType, Item, Plan, Publisher, ResourceMap,
                Scheme, Subscriber, Subscription, Topic, User }

/**
 * Views are just Scala functions so we can test them directly when Integration
 # tests are unwieldy due to, for example, external API calls.
 */
class SearchViewsSpec extends Specification {

  "Search Index" should {
    "render search index template" in new WithApplication(
        FakeApplication(additionalConfiguration = inMemoryDatabase())){
      val fakeRequest = FakeRequest(GET, controllers.routes.Search.index().url, FakeHeaders(), "")
      val html = views.html.search.index()(fakeRequest)
      contentAsString(html) must contain("Search for stuff!")
    }
  }

  "Topic Search" should {
    "render search results template with no topics and no subscriber" in new WithApplication(
        FakeApplication(additionalConfiguration = inMemoryDatabase())){
      val term = "popcorn"
      val target = "topic"
      val fakeRequest = FakeRequest(GET, controllers.routes.Search.results(term, target).url,
                                    FakeHeaders(), "")
      val html = views.html.search.topic_results(term, target, 0, 10, Topic.all, 25,
                                                 None)(fakeRequest)
      contentAsString(html) must contain(term)
      contentAsString(html) must contain("Viewing 1 - 0 of 25")
    }

    "render search results template with found topics and no subscriber" in
        new WithApplication(FakeApplication(additionalConfiguration = inMemoryDatabase())){
      val term = "popcorn"
      val target = "topic"
      val fakeRequest = FakeRequest(GET, controllers.routes.Search.results(term, target).url,
                                    FakeHeaders(), "")
      val s = Scheme.make("tag", "topic", "cat", "SomeScheme", Some("link"), Some("logo"))
      Topic.make(s.id, "popcorn_tag1", "popcorn_name1")
      Topic.make(s.id, "popcorn_tag2", "popcorn_name2")
      val html = views.html.search.topic_results(term, target, 0, 10, Topic.all, 25,
                                                 None)(fakeRequest)
      contentAsString(html) must contain(term)
      contentAsString(html) must contain("Viewing 1 - 2 of 2")
    }

    "render search results template with found topics and subscriber with no subscriptions" in
        new WithApplication(FakeApplication(additionalConfiguration = inMemoryDatabase())){
      val term = "popcorn"
      val target = "topic"
      val u = User.make("bob", "bob@example.com", "", "https://oidc.mit.edu/current_user")
      val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact",
                                Some("link"), Some("logo"))
      val fakeRequest = FakeRequest(GET, controllers.routes.Search.results(term, target).url,
                                    FakeHeaders(), "")
      val s = Scheme.make("tag", "topic", "cat", "SomeScheme", Some("link"), Some("logo"))
      Topic.make(s.id, "popcorn_tag1", "popcorn_name1")
      Topic.make(s.id, "popcorn_tag2", "popcorn_name2")
      val html = views.html.search.topic_results(term, target, 0, 10, Topic.all, 25,
                                                 Some(sub))(fakeRequest)
      contentAsString(html) must contain(term)
      contentAsString(html) must contain("Viewing 1 - 2 of 2")
      contentAsString(html) must contain("You are NOT subscribed to this Topic")
    }

    "render search results template with found topics and subscriber with matching subscription" in
        new WithApplication(FakeApplication(additionalConfiguration = inMemoryDatabase())){
      val term = "popcorn"
      val target = "topic"
      val u = User.make("bob", "bob@example.com", "", "https://oidc.mit.edu/current_user")
      val sub = Subscriber.make(u.id, "Sub Name", "cat", "contact",
                                Some("link"), Some("logo"))
      val chan = Channel.make(sub.id, "protocol", "mode", "description", "userid",
                              "password", "http://example.com")
      val p = Plan.make(sub.id, chan.id, "name", "description", "thumbs-up", "deliver",
                        "review", "subscribe", "review")
      val s = Scheme.make("tag", "topic", "cat", "SomeScheme", Some("link"), Some("logo"))
      p.addScheme(s)
      val t = Topic.make(s.id, "popcorn_tag1", "popcorn_name1")
      Topic.make(s.id, "popcorn_tag2", "popcorn_name2")
      Subscription.create(sub.id, t.id, "deliver", sub.created, sub.created)
      val fakeRequest = FakeRequest(GET, controllers.routes.Search.results(term, target).url,
                                    FakeHeaders(), "")
      val html = views.html.search.topic_results(term, target, 0, 10, Topic.all, 25,
                                                 Some(sub))(fakeRequest)
      contentAsString(html) must contain(term)
      contentAsString(html) must contain("Viewing 1 - 2 of 2")
      contentAsString(html) must contain("You are NOT subscribed to this Topic")
      contentAsString(html) must contain("You are subscribed to this Topic")
    }
  }

  "Item Search" should {
    "render search results template with no items" in new WithApplication(
        FakeApplication(additionalConfiguration = inMemoryDatabase())){
      val term = "popcorn"
      val target = "item"
      val fakeRequest = FakeRequest(GET, controllers.routes.Search.results(term, target).url,
                                    FakeHeaders(), "")
      val html = views.html.search.item_results(term, target, 0, 10, Item.all, 25)(fakeRequest)
      contentAsString(html) must contain(term)
      contentAsString(html) must contain("Viewing 1 - 0 of 25")
    }

    "render search results template with no items" in new WithApplication(
        FakeApplication(additionalConfiguration = inMemoryDatabase())){
      val term = "popcorn"
      val target = "item"
      val ct = ContentType.make("tag", "label", "desc", Some("logo"))
      val rm = ResourceMap.make("rm_tag", "rm_desc", Some("http://www.example.com"))
      val user = User.make("pubuser", "pubuser@example.com", "", "pub_identity")
      val pub = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus",
                               Some(""), Some(""))
      val col = Collection.make(pub.id, ct.id, rm.id, "coll1_tag", "coll1 desc", "open")
      Item.make(col.id, ct.id, "location", "abc")
      Item.make(col.id, ct.id, "location", "def")
      val fakeRequest = FakeRequest(GET, controllers.routes.Search.results(term, target).url,
                                    FakeHeaders(), "")
      val html = views.html.search.item_results(term, target, 0, 10, Item.all, 25)(fakeRequest)
      contentAsString(html) must contain(term)
      contentAsString(html) must contain("Viewing 1 - 2 of 2")
    }
  }
}
