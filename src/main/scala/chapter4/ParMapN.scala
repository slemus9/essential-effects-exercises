package chapter4

import cats.syntax.apply._
import cats.effect.{IO, Outcome, OutcomeIO, FiberIO}
import Outcome.{Succeeded, Canceled, Errored}

/*
  - Start both computations so they run concurrently ("fork them")
  - Wait for each result (join them)
  - If one effect fails, cancel the other one
  - Combine the results with the given f function
*/
object ParMapN {

  private def parMapNWrong [A, B, C] (
    fa: IO[A], fb: IO[B]
  ) (f: (A, B) => C): IO[C] =
    for {
      fiberA <- fa.start // fork the execution in a different thread
      fiberB <- fb.start
      b      <- fiberB.join // await the results of a concurrent effect with join. The execution continues on the joined thread
                      .onError { _ => fiberA.cancel }
      a      <- fiberA.join // The order in which we join doesn't matter since we need both results
                      .onError { _ => fiberB.cancel }
                      // This onError handler won't be registered until fiberB completes,
                      // which is not what we want
    } yield ???

  def parMapN [A, B, C] (
    fa: IO[A], fb: IO[B]
  ) (f: (A, B) => C): IO[C] = {

    def raiseCancelationError [A] = IO.raiseError[A](
      new InterruptedException("A fiber was cancelled")
    )

    // Every flatMap is treated as a cancelation boundary
    IO.racePair(fa, fb).flatMap {
      case Left(outA -> fiberB)  => fiberB.join.flatMap { 
        (outA, _).mapN(f).embed(raiseCancelationError)
      }
      case Right(fiberA -> outB) => fiberA.join.flatMap { 
        (_, outB).mapN(f).embed(raiseCancelationError)
      }
    }
  }

  def parMapNAlt [A, B, C] (
    fa: IO[A], fb: IO[B]
  ) (f: (A, B) => C): IO[C] = {
    
    val raiseCancelationError = IO.raiseError(
      new InterruptedException("A fiber was cancelled")
    )

    def handleRaceOutcomeWith [A, B, C] (f: (A, B) => C) (
      outA: OutcomeIO[A], fiberB: FiberIO[B]
    ) = outA match {
      case Canceled()    => fiberB.cancel >> raiseCancelationError
      case Errored(e)    => IO.raiseError(e)
      case Succeeded(fa) => 
        ( fa
        , fiberB.joinWith(raiseCancelationError)
        ).mapN(f)
    }

    val handlerA = handleRaceOutcomeWith (f) _
    val handlerB = handleRaceOutcomeWith { (b: B, a: A) => f(a, b) } _

    IO.racePair(fa, fb).flatMap {
      case Left(outA -> fiberB)  => handlerA(outA, fiberB)
      case Right(fiberA -> outB) => handlerB(outB, fiberA)
    }
  }
}