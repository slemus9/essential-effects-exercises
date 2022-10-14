package chapter10

import cats.syntax.all._
import cats.effect.{IO, OutcomeIO, Ref}

trait Reactor [A] {
  def whenAwake (
    onStart: Job.Id => IO[Unit],
    onComplete: (Job.Id, OutcomeIO[A]) => IO[Unit]
  ): IO[Unit]
}

object Reactor {

  def apply [A] (stateRef: Ref[IO, JobScheduler.State[A]]) = new Reactor[A] {

    
    def whenAwake (
      onStart: Job.Id => IO[Unit],
      onComplete: (Job.Id, OutcomeIO[A]) => IO[Unit]
    ): IO[Unit] = {

      def jobCompleted (job: Job.Completed[A]): IO[Unit] = 
        stateRef
          .update(_.onComplete(job))
          .flatTap(_ => onComplete(job.id, job.outcome).attempt)

      def registerOnComplete (job: Job.Running[A]) =
        job.await.flatMap(jobCompleted)

      def startJob (scheduled: Job.Scheduled[A]): IO[Job.Running[A]] = 
        for {
          job <- scheduled.start
          _   <- stateRef.update(_.running(job))
          _   <- registerOnComplete(job)
          _   <- onStart(job.id).attempt
        } yield job

      val startNextJob: IO[Option[Job.Running[A]]] = 
        for {
          job     <- stateRef.modify(_.dequeue)
          running <- job.traverse(startJob)
        } yield running

      startNextJob.iterateUntil(_.isEmpty).void
    }
  }
}