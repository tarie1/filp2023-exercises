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
  final private val regex                               = "\\d{4}\\s\\d{6}".r
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
  private def passportParsing(passport: Option[String]): Option[None.type] = {
    passport match {
      case None                                      => Option(None)
      case Some(value) if value.matches(regex.regex) => Option(None)
      case _                                         => None
    }
  }
  def transformToEither(rawUser: RawUser): Either[Error, User] =
    for {
      ban        <- if (!rawUser.banned) Right(()) else Left(Banned)
      id         <- Either.fromOption(rawUser.id.toLongOption)(InvalidId)
      firstName  <- Either.fromOption(rawUser.firstName)(InvalidName)
      secondName <- Either.fromOption(rawUser.secondName)(InvalidName)
      passport   <- Either.fromOption(passportParsing(rawUser.passport))(InvalidPassport)
      pass = rawUser.passport match {
        case Some(value) => Option(Passport(value.split(" ")(0).toLong, value.split(" ")(1).toLong))
        case None        => None
      }
    } yield User(id, UserName(firstName, secondName, rawUser.thirdName), pass)
}
