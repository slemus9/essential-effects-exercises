package chapter9

import cats.syntax.parallel._
import cats.effect.{IO, IOApp, Deferred, Ref}
import utils.debug._
import scala.concurrent.duration._

object IsThirteenLatch extends IOApp.Simple {

  def beeper (latch: CountdownLatch) = 
    latch.await >> IO("BEEP!").debug

  def tickingClock (latch: CountdownLatch): IO[Unit] =
    IO.sleep(1.second) >> 
    IO(System.currentTimeMillis).debug >>
    latch.decrement >> 
    tickingClock(latch)

  def run: IO[Unit] = CountdownLatch(13).flatMap { latch => 
    ( beeper(latch)
    , tickingClock(latch)
    ).parTupled.void  
  }
}