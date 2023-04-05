package exercises05.parser

import exercises05.either.EitherCombinators._
import Error._

object Examples {

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть None
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть None
    * если rawUser.id не парсится в Long то функция должна вернуть None
    * если rawUser.banned, то вернуть None
    * используйте for-comprehension
    */
  final private val passportFormat                      = "(\\d{4}) (\\d{6})".r
  def transformToOption(rawUser: RawUser): Option[User] = Either.toOption(transformToEither(rawUser))

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть Left(InvalidName)
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть Left(InvalidPassport)
    * если rawUser.id не парсится в Long то функция должна вернуть Left(InvalidId)
    * если rawUser.banned, то вернуть Left(Banned)
    * у ошибок есть приоритет:
    * 1. Banned
    * 2. InvalidId
    * 3. InvalidName
    * 4. InvalidPassport
    * используйте for-comprehension
    * но для того, чтобы for-comprehension заработал надо реализовать map и flatMap в Either
    */
  private def passportParsing(passport: String): Option[Passport] = {
    passport match {
      case passportFormat(series, number) => Some(Passport(series.toLong, number.toLong))
      case _                              => None
    }
  }
  def transformToEither(rawUser: RawUser): Either[Error, User] =
    for {
      ban        <- if (!rawUser.banned) Right(()) else Left(Banned)
      id         <- Either.fromOption(rawUser.id.toLongOption)(InvalidId)
      firstName  <- Either.fromOption(rawUser.firstName)(InvalidName)
      secondName <- Either.fromOption(rawUser.secondName)(InvalidName)
      passport <- Either.fromOption(
        if (rawUser.passport.isEmpty) Some(None) else rawUser.passport.flatMap(passportParsing).map(Some(_))
      )(InvalidPassport)
    } yield User(id, UserName(firstName, secondName, rawUser.thirdName), passport)
}
