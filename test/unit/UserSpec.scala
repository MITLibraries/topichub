import org.specs2.mutable._
import org.specs2.matcher._
import org.specs2.matcher.MatchResult
import play.api.test._
import play.api.test.Helpers._
import models.Publisher
import models.User

class UserSpec extends Specification {

  "User model" should {

    "#create" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.findById(1) must equalTo(None)
        User.create("bob", "bob@example.com", "role1, role2", "identity")
        User.findById(1).get.name must equalTo("bob")
      }
    }

    "#make" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.findById(1) must equalTo(None)
        val user = User.make("bob", "bob@example.com", "role1, role2", "identity")
        user.id must equalTo(1)
      }
    }

    "#findById" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("bob", "bob@example.com", "role1, role2", "identity")
        User.findById(user.id).get must equalTo(user)
      }
    }

    "#findByName" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("bob", "bob@example.com", "role1, role2", "identity")
        User.findByName(user.name).get must equalTo(user)
      }
    }

    "#findByIdentity" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.findByIdentity("identity") must equalTo(None)
        val user = User.make("bob", "bob@example.com", "role1, role2", "identity")
        User.findByIdentity(user.identity).get must equalTo(user)
      }
    }

    "#isValidIdentity" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        User.isValidIdentity("identity") must equalTo(false)
        val user = User.make("bob", "bob@example.com", "role1, role2", "identity")
        User.isValidIdentity(user.identity) must equalTo(true)
      }
    }

    "#hasPublisher" in {
      running(FakeApplication(additionalConfiguration = inMemoryDatabase())) {
        val user = User.make("bob", "bob@example.com", "role1, role2", "identity")
        user.hasPublisher(1) must equalTo(false)

        val p = Publisher.make(user.id, "pubtag", "pubname", "pubdesc", "pubcat", "pubstatus", Some(""), Some(""))
        user.hasPublisher(1) must equalTo(true)
      }
    }
  }
}
