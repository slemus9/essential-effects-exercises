package utils

import cats.effect.IO

/*
  From https://github.com/inner-product/essential-effects-code/tree/exercises/exercises/src/main/scala/com.innerproduct.ee
*/
object debug {

  implicit class DebugHelper [A] (io: IO[A]) {

    def debug: IO[A] = io.flatTap { a => 
      val t = Thread.currentThread.getName
      IO.println(s"[${Colorize.reversed(t)}] $a")  
    }
  }
}