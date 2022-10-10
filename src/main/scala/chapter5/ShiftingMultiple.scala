package chapter5

import cats.effect.{IO, IOApp}
import utils.debug._
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors

object ShiftingMultiple extends IOApp.Simple {

  def ec (name: String): ExecutionContext = 
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor { r => 
      val t = new Thread(r, s"pool-$name-thread-1")
      t.setDaemon(true)
      t
    })
 
  def run: IO[Unit] = {
    val ec1 = ec("1")
    val ec2 = ec("2")

    IO("one").debug.evalOn(ec1) >> 
    IO("two").debug.evalOn(ec2) >>
    IO("three").debug.void
  }
}