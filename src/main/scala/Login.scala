sealed trait LoginError

case object InvalidPassword extends LoginError

case object UserNotFound extends LoginError

case object PasswordLocked extends LoginError

case class User(id: Long, name: String, password: String)

object LoginService {
  def login(name: String, password: String): Either[LoginError, User] = {
    if (password == "invalid")
      Left(InvalidPassword)
    else if (name == "notfound")
      Left(UserNotFound)
    else if (password == "locked")
      Left(PasswordLocked)
    else
      Right(User(1, name, password))
  }
}
