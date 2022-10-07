package chapter2

import cats.effect.{IOApp, IO, ExitCode}
import scala.concurrent.duration._
import java.time.LocalDateTime
import scala.util.Try

object TickingClock extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    tickingClock.as(ExitCode.Success)

  val getCurrentTime: IO[LocalDateTime] = 
    IO { LocalDateTime.now() }

  val tickingClock: IO[Unit] = {
    val printCurrentTime = getCurrentTime.flatMap { t => 
      val h = t.getHour
      val m = t.getMinute
      val s = t.getSecond
      IO.println(s"Current Time: $h:$m:$s")  
    }

    printCurrentTime   >> 
    IO.sleep(1.second) >> 
    tickingClock
  } // <1>
}