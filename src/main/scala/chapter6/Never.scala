package chapter6

import cats.effect.{IOApp, IO}
import utils.debug._

object Never extends IOApp.Simple {

  val never: IO[Nothing] = IO.async_ { _ => () }

  def run: IO[Unit] = 
    never.guarantee(IO("I guess never is now").debug.void)
    
  IO.never
}