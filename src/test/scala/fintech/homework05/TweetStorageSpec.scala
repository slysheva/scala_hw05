package fintech.homework05

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

class TweetStorageSpec extends FlatSpec with Matchers {

  val tweet = Tweet("42",
    "Barney",
    "It's gonns be LEGEND wait for it ... DARY Legendary!!! #SuitUp #Legendary #BeAwesome",
    Seq("#SuitUp", "#Legendary", "#BeAwesome"),
    Some(Instant.now),
    0)

  it should "save tweet correctly" in {
    val storage = new Storage(Map.empty)
    storage.saveTweet(tweet) should be(Success(tweet))
  }

  it should "get tweet correctly" in {
    val storage = new Storage(Map.empty)
    storage.saveTweet(tweet)
    val returnedTweet = storage.getTweet(tweet.id)
    returnedTweet should be(Success(tweet))
    returnedTweet match {
      case Success(result) => result should be equals (tweet)
    }
  }

  it should "delete tweet correctly" in {
    val storage = new Storage(Map.empty)
    storage.saveTweet(tweet)
    val returnedTweet = storage.deleteTweet(tweet.id)
    returnedTweet should be(Success(tweet))
    returnedTweet match {
      case Success(result) => result should be equals (tweet)
    }
  }

  it should "not save tweet twice" in {
    val storage = new Storage(Map.empty)
    storage.saveTweet(tweet)
    val res = storage.saveTweet(tweet)
    res should be(Error("Tweet is already exist"))
  }

  it should "not delete tweet twice" in {
    val storage = new Storage(Map.empty)
    storage.saveTweet(tweet)
    storage.deleteTweet(tweet.id)
    val res = storage.deleteTweet(tweet.id)
    res should be(Error("There is no tweet with given id"))
  }

  it should "return error when there is no such tweet to get" in {
    val storage = new Storage(Map.empty)
    storage.getTweet("671") should be(Error("There is no tweet with given id"))
  }
}