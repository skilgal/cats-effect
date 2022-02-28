package com.rockthejvm.part3fibers

import cats.effect.kernel.Fiber
import cats.effect.IOApp
import cats.effect.IO
import cats.instances.duration
import cats.syntax.flatMap.*
import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.Errored

import scala.concurrent.duration.FiniteDuration
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Canceled
import cats.syntax.flatMap

import java.lang

object Fibers extends IOApp.Simple {

  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val comp = for {
      fib    <- io.start
      result <- fib.join
    } yield result

    comp.flatMap {
      case Succeeded(fa) => fa
      case Errored(e)    => IO.raiseError(e)
      case Canceled() =>
        IO.raiseError(new RuntimeException("Computation canceled"))
    }
  }

  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val comp = for {
      fiberA <- ioa.start
      fiberB <- iob.start
      aRes   <- fiberA.join
      bRes   <- fiberB.join
    } yield (aRes, bRes)

    comp flatMap {
      case (Succeeded(a), Succeeded(b)) =>
        for {
          resA <- a
          resB <- b
        } yield (resA, resB)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _               => IO.raiseError(new RuntimeException("Computation canceled"))
    }
  }

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val comp = for {
      fib    <- io.start
      _      <- IO.sleep(duration) >> fib.cancel
      result <- fib.join
    } yield result

    comp flatMap {
      case Succeeded(fa) => fa
      case Errored(e)    => IO.raiseError(e)
      case Canceled()    => IO.raiseError(new RuntimeException("Computation canceled"))
    }
  }

  def run: IO[Unit] = ???
}
