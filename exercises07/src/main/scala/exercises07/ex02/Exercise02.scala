package exercises07.ex02

import exercises07.data.NonEmptyList
import exercises07.ex02.Domain._
import exercises07.ex02.Errors._
import exercises07.typeclasses._

object Exercise02 {
  type TransformationSupport[F[_]] = ApplicativeError[F, NonEmptyList[ParsingError]]

  object TransformationSupport {
    @inline
    def apply[F[_]](implicit inst: TransformationSupport[F]): TransformationSupport[F] =
      inst
  }

  private implicit class OptionOps[A](private val opt: Option[A]) extends AnyVal {
    def require[F[_]](err: => ParsingError)(implicit ts: TransformationSupport[F]): F[A] =
      opt match {
        case Some(value) => ts.pure(value)
        case None        => ts.raiseError(NonEmptyList.of(err))
      }
  }

  // Советуем воспользоваться)
  import TupleSyntax._
  import TransformerSyntax._
  import exercises07.ex01.Exercise01.Syntax._
  import exercises07.ex01.Exercise01.Instances._

  implicit def personTransformerF[F[_]: TransformationSupport]: TransformerF[F, RawPerson, Person] =
    (rawPerson: RawPerson) => {
      val id    = rawPerson.id.toLongOption.require(InvalidPersonId(rawPerson.id))
      val name  = rawPerson.name.require(MissingPersonName)
      val phone = Phone.parse(rawPerson.phone).require(InvalidPhone(rawPerson.phone))
      (id, name, phone).mapN(Person)
    }

  implicit def addressBookTransformerF[F[_]: TransformationSupport]: TransformerF[F, RawAddressBook, AddressBook] =
    (rawAddressBook: RawAddressBook) => {
      val id      = rawAddressBook.id.toLongOption.require(InvalidAddressBookId(rawAddressBook.id))
      val persons = rawAddressBook.persons.traverse(personTransformerF[F].transform)
      (id, persons).mapN(AddressBook)
    }
}
