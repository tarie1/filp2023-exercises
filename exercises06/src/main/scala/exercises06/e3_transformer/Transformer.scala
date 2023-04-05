package exercises06.e3_transformer

import exercises06.e3_transformer.Error.{InvalidId, InvalidName}

trait Transformer[A, B] {
  def toOption(a: A): Option[B]
  def toEither(a: A): Either[Error, B]
}

object Either {
  //пользуюсь методом из предыдущей домашки
  def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] = option match {
    case Some(value) => Right(value)
    case None        => Left(a)
  }
}

object TransformerInstances {
  implicit val transformer: Transformer[RawUser, User] = new Transformer[RawUser, User] {
    def toOption(rawUser: RawUser): Option[User] =
      for {
        id         <- rawUser.id.toLongOption
        firstName  <- rawUser.firstName
        secondName <- rawUser.secondName
        thirdName = rawUser.thirdName
      } yield User(id, UserName(firstName, secondName, thirdName))

    def toEither(rawUser: RawUser): Either[Error, User] =
      for {
        id         <- Either.fromOption(rawUser.id.toLongOption)(InvalidId)
        firstName  <- Either.fromOption(rawUser.firstName)(InvalidName)
        secondName <- Either.fromOption(rawUser.secondName)(InvalidName)
        thirdName = rawUser.thirdName
      } yield User(id, UserName(firstName, secondName, thirdName))
  }
}

object TransformerSyntax {
  implicit class TransformerOps[A](private val value: A) {
    def transformToOption[B](implicit transformer: Transformer[A, B]): Option[B]        = transformer.toOption(value)
    def transformToEither[B](implicit transformer: Transformer[A, B]): Either[Error, B] = transformer.toEither(value)
  }
}

object Examples {
  import TransformerInstances._
  import TransformerSyntax._
  RawUser("1234", Some(""), Some(""), None).transformToOption[User]
  RawUser("1234", Some(""), Some(""), None).transformToEither[User]
}
