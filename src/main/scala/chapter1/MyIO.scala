package chapter1

import scala.util.Try
import scala.annotation.tailrec

/*
  Custom IO monad for sequential computations with stack 
  safety
*/
sealed trait MyIO [+A] { self => 

  def map [B] (f: A => B): MyIO[B]

  def flatMap [B] (f: A => MyIO[B]): MyIO[B] 

  def *> [B] (another: MyIO[B]): MyIO[B] = flatMap { _ => another }

  def as [B] (newValue: => B): MyIO[B] = map { _ => newValue } 

  def void: MyIO[Unit] = as { () } 

  def attempt: MyIO[Either[Throwable, A]]

  def option: MyIO[Option[A]] = attempt.map { _.toOption }

  def handleErrorWith [AA >: A] (f: Throwable => MyIO[AA]): MyIO[AA] = 
    attempt.flatMap { _.fold(f, MyIO.pure) }

  def redeem [B] (recover: Throwable => B, map: A => B): MyIO[B] = 
    attempt.map { _.fold(recover, map) }

  def redeemWith [B] (recover: Throwable => MyIO[B], bind: A => MyIO[B]): MyIO[B] = 
    attempt.flatMap { _.fold(recover, bind) }

  def unsafeRun (): A = MyIO.runTrampolined(self)
}

private final case class IOResult [+A] (val value: A) extends MyIO[A] {

  override def map [B] (f: A => B): MyIO[B] = MyIO { f(value) }

  override def flatMap [B] (f: A => MyIO[B]): MyIO[B] = MyIO.suspend { f(value) } 

  override def attempt: MyIO[Either[Throwable,A]] = MyIO.pure(Right(value))
}

private final case class IOExec [+A] (val run: () => A) extends MyIO[A] {

  override def map [B] (f: A => B): MyIO[B] = MyIO { f(run()) }

  override def flatMap [B] (f: A => MyIO[B]): MyIO[B] = MyIO.suspend { f(run()) } 

  override def attempt: MyIO[Either[Throwable,A]] = MyIO { Try(run()).toEither }
}

private final case class IOSuspend [+A] (val run: () => MyIO[A]) extends MyIO[A] {

  override def map [B] (f: A => B): MyIO[B] = MyIO.suspend { run().map(f) }

  override def flatMap [B] (f: A => MyIO[B]): MyIO[B] = MyIO.suspend { run().flatMap(f) }

  override def attempt: MyIO[Either[Throwable,A]] = MyIO.suspend {
    Try(run()).toEither.fold(
      e   => MyIO(Left(e)),
      Myio  => Myio.attempt
    )
  }
}

object MyIO {

  def apply [A] (body: => A): MyIO[A] = IOExec(() => body)

  def suspend [A] (thunk: => MyIO[A]): MyIO[A] = IOSuspend(() => thunk)

  def delay [A] (body: => A): MyIO[A] = MyIO(body)

  def pure [A] (a: A): MyIO[A] = IOResult(a)

  def fromEither [A] (e: Either[Throwable, A]): MyIO[A] = 
    e.fold(MyIO.raiseError, MyIO.pure)

  def fromOption [A] (option: Option[A])(orElse: => Throwable): MyIO[A] = 
    option.fold (MyIO.raiseError[A](orElse)) (MyIO.pure)

  def fromTry [A] (t: Try[A]): MyIO[A] = 
    fromEither(t.toEither)

  def none [A]: MyIO[Option[A]] = MyIO.pure(None)

  def raiseError [A] (e: Throwable): MyIO[A] = MyIO(throw e)

  def raiseUnless (cond: Boolean) (e: => Throwable): MyIO[Unit] = 
    unlessA (cond) (raiseError(e))

  def raiseWhen (cond: Boolean) (e: => Throwable): MyIO[Unit] = 
    whenA (cond) (raiseError(e))

  def unlessA (cond: Boolean) (action: => MyIO[Unit]): MyIO[Unit] = 
    if (!cond) action else MyIO.unit

  def whenA (cond: Boolean) (action: => MyIO[Unit]): MyIO[Unit] = 
    if (cond) action else MyIO.unit

  val unit: MyIO[Unit] = MyIO.pure(())

  def putStr (s: String) = MyIO { println(s) }

  @tailrec
  private def runTrampolined [A] (io: MyIO[A]): A = io match {
    case IOResult(v)    => v
    case IOExec(run)    => run() 
    case IOSuspend(run) => runTrampolined(run())
  }
}


object IOExamples extends App {

  def fib (n: Int, a: Long = 0, b: Long = 1): MyIO[Long] =
    MyIO(a + b).flatMap { b1 =>
      if (n > 0) 
        fib(n - 1, b, b1)
      else 
        MyIO.pure(a)
  }

  println(fib(100000).unsafeRun())
  println(
    (MyIO.pure(42) *> MyIO(throw new RuntimeException("error"))).attempt.unsafeRun()
  )
}