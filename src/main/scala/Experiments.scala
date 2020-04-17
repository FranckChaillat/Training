import utils.Utils

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Experiments extends App {

  val test = Seq(Future(1), Future.failed(new IllegalArgumentException("")))


}
