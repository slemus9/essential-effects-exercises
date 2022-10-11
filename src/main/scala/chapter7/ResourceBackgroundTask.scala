package chapter7

import cats.effect.{IO, IOApp, Resource}
import utils.debug._
import scala.concurrent.duration._

object ResourceBackgroundTask extends IOApp.Simple {

  val backgroundTask: Resource[IO, Unit] = {
    val loop = (
      IO("looping").debug *> IO.sleep(100.millis)
    ).foreverM

    Resource.make (
      IO("> forking background task").debug *> loop.start
    ) (
      IO("< cancelling background task").debug.void *> _.cancel
    ).map(_ => ())
  }

  def run: IO[Unit] = backgroundTask.use { _ => 
    IO("other work while background task is running").debug *>
    IO.sleep(200.millis) *> 
    IO("other work done").debug  
  } *> IO("all done").debug.void
}