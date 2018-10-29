package fintech.homework05

import java.time.Instant
import java.util.UUID

import com.sun.net.httpserver.Authenticator

import scala.util.matching.Regex
import javax.naming.spi.DirStateFactory

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Result[T]
  * в котором может лежать либо текст ошибки, либо результат выполнение
  */

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int)

case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)

final case class Success[T](result: T) extends Result[T]

final case class Error[T](errorMessage: String) extends Result[T]

sealed trait Result[+T]{
  val value: Any = this match {
    case Success(result) => result
    case Error(errorMessage) => errorMessage
  }
}

trait TweetStorage {
  def saveTweet(tweet: Tweet): Result[Tweet]
  def getTweet(id: String): Result[Tweet]
  def deleteTweet(id: String): Result[Tweet]
}

class Storage(var storage: Map[String, Tweet]) extends  TweetStorage {

  override def saveTweet(tweet: Tweet): Result[Tweet] = {
    if (storage contains tweet.id) {
      Error("Tweet is already exist")
    }
    else {
      storage += (tweet.id -> tweet)
      Success(tweet)
    }
  }

  override def getTweet(id: String): Result[Tweet] = {
    if (storage contains id) {
      Success(storage(id))
    }
    else {
      Error("There is no tweet with given id")
    }
  }

  override def deleteTweet(id: String): Result[Tweet] = {
    if (storage contains id) {
      val tweet = storage(id)
      storage -= id
      Success(tweet)
    }
    else {
      Error("There is no tweet with given id")
    }
  }
}

class TweetApi(storage: TweetStorage) {
  val maxTweetLength = 280

  def createTweet(request: CreateTweetRequest): Result[Tweet] = {
      if (request.text.length() > maxTweetLength)
        Error("Tweet text is too long")
      else {
        val id = UUID.randomUUID.toString
        val user = request.user
        val text = request.text
        val reg : Regex = "(?<=[\\s>]|^)#(\\w*[A-Za-z_]+\\w*)".r
        val hashTags = (for (each <- reg.findAllMatchIn(text)) yield each.toString()).toSeq
        val createdAt = Some(Instant.now())
        val likes = 0
        storage.saveTweet(Tweet(id, user, text, hashTags, createdAt, likes))
      }
  }

  def getTweet(request: GetTweetRequest): Result[Tweet] = {
      storage.getTweet(request.id)
  }

  def likeTweet(request: LikeRequest): Result[Tweet] = {
    val deleted = storage.deleteTweet(request.id)
    deleted match {
      case Success(result) =>
        val tweet = result
        storage.saveTweet(tweet.copy(likes = tweet.likes + 1))
      case Error(errorMsg) =>
        Error(errorMsg)
    }
  }
}

object TweetApiExample extends App {

  val storage: TweetStorage = new Storage(Map.empty)
  val app = new TweetApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val response = app.createTweet(request)
  response match {
    case Success(value) => println(s"Created tweet with id: ${value.id}")
    case Error(message) => println(s"Failed to create tweet: $message")
  }

}
