package chapter9

import cats.syntax.parallel._
import cats.effect.{IO, IOApp, Deferred, Ref}
import utils.debug._

trait CountdownLatch {
  def await: IO[Unit]
  def decrement: IO[Unit]
}

object CountdownLatch {

  sealed trait State
  final case class Outstanding (
    n: Long, 
    whenDone: Deferred[IO, Unit]
  ) extends State
  final case object Done extends State

  def apply (n: Long): IO[CountdownLatch] = for {
    whenDone <- Deferred[IO, Unit]
    state    <- Ref[IO].of[State](Outstanding(
      n, whenDone
    ))
  } yield new CountdownLatch {

    def await: IO[Unit] = state.get.flatMap {
      case Outstanding(_, whenDone) => whenDone.get
      case Done                     => IO.unit 
    }

    def decrement: IO[Unit] = state.modify {
      case Outstanding(1, whenDone) => 
        Done -> whenDone.complete(()).void
      case Outstanding(n, whenDone) => 
        Outstanding(n - 1, whenDone) -> IO.unit
      case Done                     => 
        Done -> IO.unit
    }.flatten.uncancelable
  } 
}

object CountdownLatchExample extends IOApp.Simple {

  def actionWithPrerequisites (latch: CountdownLatch): IO[String] =
    IO("Waiting for prerequisites").debug >>
    latch.await >>
    IO("action").debug


  def runPrerequisite (latch: CountdownLatch): IO[String] = 
    IO("prerequisite").debug <* latch.decrement

  def run: IO[Unit] = 
    CountdownLatch(1).flatMap { latch => 
      ( actionWithPrerequisites(latch)
      , runPrerequisite(latch)
      ).parTupled.void
    }
}