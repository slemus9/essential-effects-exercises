package chapter1

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

object Timing extends App {
  val clock: MyIO[Long] =
    MyIO { System.currentTimeMillis } // <1>

  def time[A](action: MyIO[A]): MyIO[(FiniteDuration, A)] =
    for {
      t0 <- clock.map(_.millis)
      a  <- action
      t1 <- clock.map(_.millis)
    } yield (t1 - t0, a) // <2>

  val timedHello = Timing.time(MyIO.putStr("hello"))

  timedHello.unsafeRun() match {
    case (duration, _) => println(s"'hello' took $duration")
  }
}