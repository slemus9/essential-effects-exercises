package chapter10

import cats.effect.{IO, IOApp, Ref, Deferred}
import cats.evidence.As

trait Zzz {
  def sleep: IO[Unit] // semantically block unitl wakeUp is invoked
  def wakeUp: IO[Unit] // semantically unblock any sleepers. No effect if already awake
}

object Zzz {

  sealed trait SleepingState
  final case object Asleep extends SleepingState
  final case object Awake  extends SleepingState

  type Asleep = Asleep.type
  type Awake  = Awake.type

  final case class State (
    isSleeping: SleepingState,
    sleeping: Deferred[IO, Unit]
  )

  object State {
    def awake: IO[Ref[IO, State]] = Deferred[IO, Unit].flatMap {
      sleeping => Ref.of(State(Awake, sleeping))
    }
  }

  def awake: IO[Zzz] = State.awake.map(create)

  def create (stateR: Ref[IO, State]): Zzz = new Zzz {

    def sleep: IO[Unit] = stateR.get.flatMap { state => 
      state.isSleeping match {
        case Asleep => IO.unit
        case Awake  => 
          val asleep = state.copy(isSleeping = Asleep)
          stateR.set(asleep) >> state.sleeping.get
      }
    }

    def wakeUp: IO[Unit] = stateR.get.flatMap { state => 
      state.isSleeping match {
        case Asleep => Deferred[IO, Unit].flatMap { sleeping =>
          val awake = State(Awake, sleeping)
          state.sleeping.complete(()) >> stateR.set(awake)
        }
        case Awake  => IO.unit 
      }
    }
  }
}

object ZzzExample extends IOApp.Simple {

  import cats.syntax.parallel._
  import utils.debug._
  import scala.concurrent.duration._

  def task1 (zzz: Zzz) = 
    IO.println("Task 1. Sleeping") >> 
    zzz.sleep >>
    IO.pure(1).debug

  def task2 (zzz: Zzz) =
    IO.println("Task 2. Awake after 4 seconds") >>
    IO.sleep(4.seconds) >>
    zzz.wakeUp >>
    IO.pure(2).debug

  def task3 (zzz: Zzz) =
    IO.println("Task 3. Try to sleep after two seconds") >>
    IO.sleep(2.seconds) >>
    zzz.sleep >> // should not sleep
    IO.pure(3).debug

  def run: IO[Unit] = 
    for {
      zzz <- Zzz.awake
      _   <- (task1(zzz), task2(zzz)).parTupled
      _   <- (task1(zzz), task3(zzz), task2(zzz)).parTupled
    } yield ()
}