package chapter10

import cats.effect.{IO, IOApp, Ref, Deferred}

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

  def create: IO[Zzz] = State.awake.map(stateR => new Zzz {

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
  })
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
    IO.println("Task 2. Awake after 3 seconds") >>
    IO.sleep(3.seconds) >>
    zzz.wakeUp >>
    IO.pure(2).debug

  def run: IO[Unit] = 
    for {
      zzz <- Zzz.create
      _   <- (task1(zzz), task2(zzz)).parTupled
      _   <- (task1(zzz), task2(zzz)).parTupled
    } yield ()
}