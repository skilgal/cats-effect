package com.rockthejvm.part3fibers

import cats.effect.*
import cats.effect.kernel.Outcome.Succeeded

import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled
import scala.util.Success

object Races extends IOApp.Simple {

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    io.race(IO.sleep(duration)).flatMap {
      case Left(_)  => io
      case Right(_) => IO.raiseError(new TimeoutException())
    }

  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibB)) =>
        fibB.join.flatMap {
          case Succeeded(resultEffect) => resultEffect.map(Right.apply)
          case Errored(e)              => IO.raiseError(e)
          case Canceled()              => IO.raiseError(new RuntimeException("Loser canceled"))
        }
      case Right((fibA, _)) =>
        fibA.join.flatMap {
          case Succeeded(resultEffect) => resultEffect.map(Left.apply)
          case Errored(e)              => IO.raiseError(e)
          case Canceled()              => IO.raiseError(new RuntimeException("Loser canceled"))
        }
    }
  }

  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) =>
        outA match {
          case Succeeded(effectA) => fibB.cancel >> effectA.map(Left.apply)
          case Errored(e)         => fibB.cancel >> IO.raiseError(e)
          case Canceled() =>
            fibB.join.flatMap {
              case Succeeded(effectB) => effectB.map(Right.apply)
              case Errored(e)         => IO.raiseError(e)
              case Canceled() => IO.raiseError(new RuntimeException("Both computations canceled."))
            }

        }
      case Right((fibA, outB)) =>
        outB match {
          case Succeeded(effectB) => fibA.cancel >> effectB.map(Right.apply)
          case Errored(e)         => fibA.cancel >> IO.raiseError(e)
          case Canceled() =>
            fibA.join.flatMap {
              case Succeeded(effectA) => effectA.map(Left.apply)
              case Errored(e)         => IO.raiseError(e)
              case Canceled() => IO.raiseError(new RuntimeException("Both computations canceled."))
            }

        }
    }

  }

  override def run = ???

}
