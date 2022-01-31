package com.rockthejvm.part3fibers

import cats.effect.IOApp
import cats.effect.IO
import scala.concurrent.ExecutionContext
import com.rockthejvm._
import scala.util.Try

object AsyncIOs extends IOApp.Simple {

  type Callback[A] = Either[Throwable, A] => Unit
  given ec: ExecutionContext = ExecutionContext.global

  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = {
    IO.async_ { (cb: Callback[A]) =>
      ec.execute(() =>
        cb {
          Try(computation()).toEither
        }
      )
    }
  }

  val compution: () => Int = () => 42

  val neverEnding: IO[Unit] = IO.async_(_ => ())

  override def run = asyncToIO(compution)(ec).debug.void

}
