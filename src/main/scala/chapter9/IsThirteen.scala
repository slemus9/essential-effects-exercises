package chapter9

import cats.syntax.parallel._
import cats.effect.{IO, IOApp, Deferred, Ref}
import utils.debug._
import scala.concurrent.duration._

object IsThirteen extends IOApp.Simple {

  def tickingClock (
    ticks: Ref[IO, Long]
    , is13: Deferred[IO, Unit]
  ): IO[Unit] = for {
    _ <- IO.sleep(1.second)
    _ <- IO(System.currentTimeMillis).debug
    c <- ticks.updateAndGet(_ + 1)
    _ <- if (c >= 13) is13.complete(()) else IO.unit
    _ <- tickingClock(ticks, is13)
  } yield ()

  def beepWhen13 (is13: Deferred[IO, Unit]) =
    is13.get >> IO("!BEEP!").debug
    
  def run: IO[Unit] = for {
    ticks <- Ref[IO].of(0L)
    is13  <- Deferred[IO, Unit]
    _     <- (
      beepWhen13(is13), tickingClock(ticks, is13)
    ).parTupled
  } yield ()
}