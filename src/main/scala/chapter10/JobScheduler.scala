package chapter10

import cats.effect.{IO, IOApp, Ref, Resource, OutcomeIO}
import cats.data.Chain

/*
  TODO: Make an implementation using semaphore/mutex for control
  TODO: Make a callback based implementation
*/
trait JobScheduler [A] {
  def schedule (task: IO[A]): IO[Job.Id]
}

object JobScheduler {

  final case class State [A] (
    maxRunning: Int,
    scheduled: Chain[Job.Scheduled[A]],
    running: Map[Job.Id, Job.Running[A]],
    completed: Chain[Job.Completed[A]]   
  ) {

    def enqueue (job: Job.Scheduled[A]): State[A] = 
      this.copy(scheduled = scheduled :+ job)


    def dequeue: (State[A], Option[Job.Scheduled[A]]) = 
      if (running.size >= maxRunning) this -> None
      else scheduled.uncons match {
        case Some(a -> rem) => this.copy(scheduled = rem) -> Some(a)
        case None           => this -> None 
      }

    def running (job: Job.Running[A]): State[A] = 
      this.copy(running = running + (job.id -> job))

    def onComplete (job: Job.Completed[A]): State[A] =
      if (running.contains(job.id)) {
        val newRunning = running - job.id
        val newCompleted = completed :+ job
        this.copy(running = newRunning, completed = newCompleted)
      }
      else this
  }

  object State {
    def initial [A] (maxRunning: Int): State[A] = State(
      maxRunning,
      Chain.empty,
      Map.empty,
      Chain.empty
    )
  }

  private def create [A] (
    stateRef: Ref[IO, State[A]],
    zzz: Zzz
  ) = new JobScheduler[A] {
    def schedule (task: IO[A]): IO[Job.Id] = 
      for {
        job <- Job.create(task)
        _   <- stateRef.update(_.enqueue(job))
        _   <- zzz.wakeUp // When the job is scheduled, wake up
      } yield job.id
  }

  def resource [A] (
    maxRunning: Int
  ): IO[Resource[IO, JobScheduler[A]]] = 
    for {
      stateRef   <- Ref[IO].of(State.initial[A](maxRunning))
      zzz        <- Zzz.awake
      scheduler  =  create(stateRef, zzz)
      reactor    =  Reactor(stateRef)
      whenAwake  =  reactor.whenAwake(IO.println, (_, _) => zzz.wakeUp)
      loop       =  (zzz.sleep *> whenAwake).foreverM
    } yield loop.background.map(_ => scheduler)
}

object JobSchedulerExample extends IOApp.Simple {

  import cats.syntax.traverse._
  import cats.effect.Deferred
  import cats.effect.std.Random
  import utils.debug._
  import scala.concurrent.duration._

  def randomTask (rand: Random[IO]): IO[Int] = 
    for {
      n <- rand.betweenInt(1, 5)
      _ <- IO.sleep(n.seconds)
    } yield n

  def run: IO[Unit] = for {
    resource <- JobScheduler.resource[Int](maxRunning = 3)
    rand     <- Random.scalaUtilRandom[IO]
    tasks    =  List.range(1, 10).map(_ => randomTask(rand).debug)
    _        <- resource.use { scheduler => 
      tasks.traverse(scheduler.schedule) >> IO.sleep(40.seconds)
    }
  } yield ()
}