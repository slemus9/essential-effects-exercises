package chapter4

import cats.syntax.all._
import cats.effect.{IO, IOApp, Outcome, OutcomeIO, FiberIO}
import Outcome.{Succeeded, Canceled, Errored}
import utils.debug._
import scala.concurrent.duration._

/*
  - Start both computations so they run concurrently ("fork them")
  - Wait for each result (join them)
  - If one effect fails, cancel the other one
  - Combine the results with the given f function
*/
object ParMapN extends IOApp.Simple {

  private def parMapNWrong1 [A, B, C] (
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

  def parMapNWrong2 [A, B, C] (
    fa: IO[A], fb: IO[B]
  ) (f: (A, B) => C): IO[C] = {

    def raiseCancelationError [A] = IO.raiseError[A](
      new InterruptedException("A fiber was cancelled")
    )

    // Every flatMap is treated as a cancelation boundary
    IO.racePair(fa, fb).flatMap {
      case Left(outA -> fiberB)  => fiberB.join.flatMap { // fiberB will join regardless of the outcome of fa
        (outA, _).mapN(f).embed(raiseCancelationError)
      }
      case Right(fiberA -> outB) => fiberA.join.flatMap { 
        (_, outB).mapN(f).embed(raiseCancelationError)
      }
    }
  }

  def parMapN [A, B, C] (
    fa: IO[A], fb: IO[B]
  ) (f: (A, B) => C): IO[C] = {

    def handleRaceOutcomeWith [A, B, C] (f: (A, B) => C) (
      outA: OutcomeIO[A], fiberB: FiberIO[B]
    ): IO[C] =       
      outA match {
        case Canceled()    => fiberB.cancel >> IO.canceled >> IO.never
        case Errored(e)    => fiberB.cancel >> IO.raiseError(e)
        case Succeeded(fa) => 
          ( fa
          , fiberB.joinWith(IO.canceled >> IO.never)
          ).mapN(f)
      }
    

    val handlerA = handleRaceOutcomeWith (f) _
    val handlerB = handleRaceOutcomeWith { (b: B, a: A) => f(a, b) } _

    IO.racePair(fa, fb).flatMap {
      case Left(outA -> fiberB)  => handlerA(outA, fiberB)
      case Right(fiberA -> outB) => handlerB(outB, fiberA)
    }
  }


  def run: IO[Unit] = {
    
    val t1 = 
      IO.println("Starting Task1") *> 
      IO.sleep(4.seconds)          *> 
      IO.pure(4)                   <*
      IO.println("Task1 finished")

    val t2 = 
      IO.println("Starting Task2") *>
      IO.sleep(2.seconds)          *>
      IO.pure(2)                   <*
      IO.println("Task2 finised")

    val t2Error: IO[Int] = 
      IO.println("Starting Task2") *>
      IO.sleep(2.seconds)          *>
      IO.raiseError(new Exception("Task 2 failed")) <*
      IO.println("Task2 finised")

    val t2Cancelled =
      IO.println("Starting Task2") *> 
      IO.sleep(2.seconds)          *>
      IO.canceled                  *>
      IO.pure(2)                   <*
      IO.println("Task2 finished")

    val t3 = parMapN (t1.debug, t2.debug) (_ + _)

    val t4 = parMapN (
      t1.debug.onCancel(IO.println("Task 1 cancelled")), 
      t2Error.debug
    ) (_ + _)

    val t5 = parMapN (
      t1.debug.onCancel(IO.println("Task 1 cancelled")), 
      t2Cancelled.debug.onCancel(IO.println("Task 2 cancelled"))
    ) (_ + _)

    // t3.flatMap(IO.println)
    // t4.flatMap(IO.println)
    t5.flatMap(IO.println)
    // (t1.debug, t2Cancelled.debug)
    //   .parMapN(_ + _)
    //   .flatMap(IO.println)
  }
}