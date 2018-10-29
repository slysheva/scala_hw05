package fintech.homework05

import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {

  it should "create tweet correctly" in {
    val api = new TweetApi(new Storage(Map.empty))
    val request = CreateTweetRequest("When I get sad, I stop being sad and be awesome instead! #SuitUp #Legendary #BeAwesome",
      "Barney")
    api.createTweet(request) match {
      case Error(errorMessage) => throw new AssertionError(errorMessage)
      case Success(result) =>
        result should be(Tweet(result.id, "Barney",
          "When I get sad, I stop being sad and be awesome instead! #SuitUp #Legendary #BeAwesome",
          Seq("#SuitUp", "#Legendary", "#BeAwesome"), result.createdAt, 0))
    }
  }

  it should "get tweet correctly" in {
    val api = new TweetApi(new Storage(Map.empty))
    val request = CreateTweetRequest("When I get sad, I stop being sad and be awesome instead! #SuitUp #Legendary #BeAwesome",
      "Barney")
    api.createTweet(request) match {
      case Error(errorMessage) => throw new AssertionError(errorMessage)
      case Success(result) =>
        api.getTweet(GetTweetRequest(result.id)) match {
          case Error(errorMessage) => throw new AssertionError(errorMessage)
          case Success(getResult) =>
            getResult should be(getResult)
        }
    }
  }

  it should "like tweet correctly" in {
    val api = new TweetApi(new Storage(Map.empty))
    val request = CreateTweetRequest("When I get sad, I stop being sad and be awesome instead! #SuitUp #Legendary #BeAwesome",
      "Barney")
    api.createTweet(request) match {
      case Error(errorMessage) => throw new AssertionError(errorMessage)
      case Success(result) =>
        api.likeTweet(LikeRequest(result.id)) match {
          case Error(errorMessage) => throw new AssertionError(errorMessage)
          case Success(likeResult) =>
            likeResult should be(Tweet(likeResult.id, "Barney",
              "When I get sad, I stop being sad and be awesome instead! #SuitUp #Legendary #BeAwesome",
              Seq("#SuitUp", "#Legendary", "#BeAwesome"), likeResult.createdAt, 1))
        }
    }
  }

  it should "extract hashTags correctly" in {
    val api = new TweetApi(new Storage(Map.empty))
    val request = CreateTweetRequest("When I get sad, I stop being sad and be awesome instead! #SuitUp #Legendary #BeAwesome",
      "Barney")
    val createdResult = api.createTweet(request)
    createdResult match {
      case Error(errorMessage) => throw new AssertionError(errorMessage)
      case Success(result) =>
        result.hashTags should be(Seq("#SuitUp", "#Legendary", "#BeAwesome"))
    }
  }

  it should "not create long tweet" in {
    val api = new TweetApi(new Storage(Map.empty))
    val request = CreateTweetRequest("Very loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong tweet",
      "Me")
    api.createTweet(request) should be (Error("Tweet text is too long"))
  }
}
