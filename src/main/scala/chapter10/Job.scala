package chapter10

import cats.effect.{
  IO, FiberIO, Deferred, 
  OutcomeIO, Outcome
}
import java.util.UUID

sealed trait Job [A]

object Job {

  final case class Id (value: UUID) extends AnyVal

  final case class Scheduled [A] (
    id: Id, 
    task: IO[A]
  ) extends Job[A] {

    val start: IO[Running[A]] = for {
      out   <- Deferred[IO, OutcomeIO[A]]
      fiber <- task.guaranteeCase(
        out.complete(_).void
      ).start
    } yield Running(id, fiber, out)
  }

  final case class Running [A] (
    id: Id,
    fiber: FiberIO[A], // the effect being executed
    outcome: Deferred[IO, OutcomeIO[A]] // the value that the execution will eventually yield
  ) extends Job[A] {

    val await: IO[Completed[A]] =
      outcome.get.map(Completed(id, _))
  }

  final case class Completed [A] (
    id: Id, 
    outcome: OutcomeIO[A] // actual job result
  ) extends Job[A]

  def create [A] (task: IO[A]): IO[Scheduled[A]] = 
    IO(Id(UUID.randomUUID())).map(
      Scheduled(_, task)
    )
}