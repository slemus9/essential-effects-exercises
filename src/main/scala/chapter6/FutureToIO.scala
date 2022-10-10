package chapter6

import cats.effect.IO
import scala.concurrent.{Future, ExecutionContext}
import scala.util.Failure
import scala.util.Success

object FutureToIO {

  trait API [A] {
    def compute: Future[A]
  }

  def doSomething [A] (api: API[A]) (
    implicit ec: ExecutionContext
  ): IO[A] = IO.async_[A](cb => 
    api.compute.onComplete { 
      case Failure(e) => cb(Left(e))
      case Success(a) => cb(Right(a))
    }
  ).guarantee(IO.cede)
}