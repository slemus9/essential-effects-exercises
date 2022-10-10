package chapter5

import cats.syntax.all._
import cats.effect.{IO, IOApp}
import utils.debug._

object Parallelism extends IOApp.Simple {

  val numCpus = Runtime.getRuntime().availableProcessors()
  def task (i: Int): IO[Int] = IO.pure(i).debug
  val tasks = List.range(0, numCpus * 2).parTraverse(task)

  def run: IO[Unit] = 
    IO(s"Number of CPUs: $numCpus").debug >> tasks.debug.void
}