package service

import cats.effect.IO
import cats.syntax.all._
import service.domain._
import twitter.TwitterApi
import twitter.domain.TwitterError.{LikeAlreadyExistError, LikeNotExistError, TweetNotExistError}
import twitter.domain._

class TwitterServiceIO(api: TwitterApi) extends TwitterService[IO] {
  def tweet(user: User, text: String): IO[TweetId] = IO.async_(x => api.tweet(user, text)(x.compose(_.toEither)))

  def like(user: User, tweetId: TweetId): IO[Unit] =
    IO.async_[Unit](x => api.like(user, tweetId)(x.compose(_.toEither))).recover {
      case LikeAlreadyExistError => ()
    }

  def unlike(user: User, tweetId: TweetId): IO[Unit] =
    IO.async_[Unit](x => api.unlike(user, tweetId)(x.compose(_.toEither))).recover {
      case LikeNotExistError => ()
    }
  def getTweet(tweetId: TweetId): IO[GetTweetResponse] =
    IO.async_[TweetInfo](x => api.get(tweetId)(x.compose(_.toEither))).attempt.map {
      case Right(tweetInfo)         => GetTweetResponse.found(tweetInfo)
      case Left(TweetNotExistError) => GetTweetResponse.notFound(tweetId)
    }
  def getTweets(ids: List[TweetId]): IO[GetTweetsResponse] =
    ids.traverse(getTweet).map(
        _.foldLeft(GetTweetsResponse(Set.empty[TweetId], Set.empty[TweetInfo]))((tweets, response) =>
          response match {
            case GetTweetResponse.Found(tweetInfo)  => tweets.copy(found = tweets.found + tweetInfo)
            case GetTweetResponse.NotFound(tweetId) => tweets.copy(notFound = tweets.notFound + tweetId)
          }
        )
      )
}
