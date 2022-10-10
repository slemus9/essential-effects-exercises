package chapter6

import cats.effect.{IO, IOApp}
import utils.debug._
import java.util.concurrent.CompletableFuture
import scala.jdk.FunctionConverters._

object AsyncCompletable extends IOApp.Simple {

  def cf(): CompletableFuture[String] = 
    CompletableFuture.supplyAsync(() => "woo!")

  def fromCF [A] (cfa: IO[CompletableFuture[A]]): IO[A] =
    cfa.flatMap { fa => 
      IO.async_[A] {cb => 
      
        val handler: (A, Throwable) => Unit = (a, err) =>
          cb(Option(a).toRight(err))
        
        fa.whenComplete(handler.asJavaBiConsumer)
      }
    }

  val effect = fromCF(IO(cf()))

  def run: IO[Unit] = effect.debug.void
}