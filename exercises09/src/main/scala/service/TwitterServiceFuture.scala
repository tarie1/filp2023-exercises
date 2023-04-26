package service

import service.domain.{GetTweetResponse, GetTweetsResponse}
import twitter.TwitterApi
import twitter.domain.TwitterError.{LikeAlreadyExistError, LikeNotExistError, TweetNotExistError}
import twitter.domain.{TweetId, TweetInfo, TwitterError, User}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

class TwitterServiceFuture(api: TwitterApi)(implicit ec: ExecutionContext) extends TwitterService[Future] {
  def tweet(user: User, text: String): Future[TweetId] = {
    val promise = Promise[TweetId]
    Future(api.tweet(user, text)(promise.complete))
    promise.future
  }

  def like(user: User, tweetId: TweetId): Future[Unit] = {
    val promise = Promise[Unit]
    Future(api.like(user, tweetId)(promise.complete))
    promise.future.recover { case LikeAlreadyExistError => () }
  }

  def unlike(user: User, tweetId: TweetId): Future[Unit] = {
    val promise = Promise[Unit]
    Future(api.unlike(user, tweetId)(promise.complete))
    promise.future.recover { case LikeNotExistError => () }
  }

  def getTweet(tweetId: TweetId): Future[GetTweetResponse] = {
    val promise = Promise[GetTweetResponse]
    Future {
      api.get(tweetId) {
        case Failure(TweetNotExistError) => promise.success(GetTweetResponse.notFound(tweetId))
        case Success(tweetInfo)          => promise.success(GetTweetResponse.found(tweetInfo))
      }
    }
    promise.future
  }

  def getTweets(ids: List[TweetId]): Future[GetTweetsResponse] = {
    Future
      .traverse(ids)(getTweet)
      .map(
        _.foldLeft(GetTweetsResponse(Set.empty[TweetId], Set.empty[TweetInfo]))((tweets, response) =>
          response match {
            case GetTweetResponse.Found(info)  => tweets.copy(found = tweets.found + info)
            case GetTweetResponse.NotFound(id) => tweets.copy(notFound = tweets.notFound + id)
          }
        )
      )
  }
}
