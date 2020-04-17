package readertest

import scalaz.Reader

trait DBRepository
class UserRepo extends DBRepository  {
}

object DBRepository {

  val repository = Reader[UserRepo, UserRepo](identity)

  def getFile(userId : Int) : Reader[UserRepo, String] = Reader { usrRepo =>
    "the user profile"
  }


}
