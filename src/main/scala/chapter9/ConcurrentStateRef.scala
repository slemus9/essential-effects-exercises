package chapter9

import cats.syntax.all._
import cats.effect.{IO, IOApp, Ref}
import scala.concurrent.duration._
import utils.debug._

object ConcurrentStateRef extends IOApp.Simple {

  def printTicks (ticks: Ref[IO, Long]): IO[Unit] = 
    for {
      _ <- IO.sleep(5.seconds)
      n <- ticks.get
      _ <- IO(s"TICKS: $n").debug
      _ <- printTicks(ticks)
    } yield ()

  def tickingClock (ticks: Ref[IO, Long]): IO[Unit] =
    IO.sleep(1.second)                 >>
    IO(System.currentTimeMillis).debug >> 
    ticks.update(_ + 1)                >>
    tickingClock(ticks)

  def run: IO[Unit] = Ref[IO].of(0L).flatMap { ticks =>
    (tickingClock(ticks), printTicks(ticks)).parTupled.void
  }
}