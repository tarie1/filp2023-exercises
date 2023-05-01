package competition

import cats.Monad
import cats.syntax.all._
import cats.instances.all._
import service.TwitterService
import twitter.domain._

class CompetitionMethods[F[_]: Monad](service: TwitterService[F]) {

  /**
    * В этом методе надо:
    * Загрузить все указанные твиты
    * найти в них твиты где есть лайки указанного юзера
    * удалить эти лайки вызвав unlike
    */
  def unlikeAll(user: User, tweetIds: List[TweetId]): F[Unit] = {
    for {
      tweets <- service.getTweets(tweetIds)
      _ <- tweets.found.toList
        .filter { tweet =>
          tweet.likedBy.contains(user)
        }
        .traverse(tweet => service.unlike(user, tweet.id))
    } yield ()
  }

  /**
    * В этом методе надо:
    * Загрузить все указанные твиты
    * выбрать среди них тот твит у которого больше всего лайков или он раньше создан, если лайков одинаковое количество
    */
  def topAuthor(tweetIds: List[TweetId]): F[Option[User]] = {
    for {
      tweets <- service.getTweets(tweetIds)
      topTweet = tweets.found.toList.sortBy(x => (-x.likedBy.size, x.created)).headOption
    } yield topTweet.map(_.author)
  }
}
