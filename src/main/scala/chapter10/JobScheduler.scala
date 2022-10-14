package chapter10

import cats.effect.{IO, Ref}
import cats.data.Chain

trait JobScheduler [A] {
  def schedule (task: IO[A]): IO[Job.Id]
}

object JobScheduler {

  final case class State [A] (
    maxRunning: Int,
    scheduled: Chain[Job.Scheduled[A]]   = Chain.empty,
    running: Map[Job.Id, Job.Running[A]] = Map.empty,
    completed: Chain[Job.Completed[A]]   = Chain.empty
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

  def scheduler [A] (stateRef: Ref[IO, State[A]]) = new JobScheduler[A] {
    def schedule (task: IO[A]): IO[Job.Id] = 
      for {
        job <- Job.create(task)
        _   <- stateRef.update(_.enqueue(job))
      } yield job.id
  }
}