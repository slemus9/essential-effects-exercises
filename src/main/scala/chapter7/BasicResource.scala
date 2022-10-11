package chapter7

import cats.effect.{IO, IOApp, Resource}
import utils.debug._

object BasicResource extends IOApp.Simple {

  val stringResource: Resource[IO, String] = 
    Resource.make (
      IO("> acquiring stringResource").debug *> IO("String")
    ) (
      _ => IO("< releasing string Resource").debug.void
    )

  def run: IO[Unit] = stringResource.use { s => 
    IO(s"$s is so cool!").debug  
  }.void
}