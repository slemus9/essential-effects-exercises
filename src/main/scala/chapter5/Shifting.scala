package chapter5

import cats.effect.{IO, IOApp}
import utils.debug._

// We no longer have the IO.shift method in CE3
object Shifting extends IOApp.Simple {

  def run: IO[Unit] = for {
    _ <- IO.pure(1).debug
    _ <- IO.cede
    _ <- IO.pure(2).debug
    _ <- IO.cede
    _ <- IO.pure(3).debug
  } yield ()
}