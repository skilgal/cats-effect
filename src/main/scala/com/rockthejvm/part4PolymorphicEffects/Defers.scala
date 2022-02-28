package com.rockthejvm.part4PolymorphicEffects

import cats.effect.Deferred
import cats.effect._
import java.lang.Throwable
import com.rockthejvm._
import scala.concurrent.duration._

object Defers extends IOApp.Simple {

  /** Exercises
    *   - medium: write a small alarm notification with two simultaneous IOs
    *     - one that increments a counter every second (a clock)
    *     - one that waits for the counter to become 10, then prints a message "time's up!"
    */

  def alarm: IO[Unit] = {

    def incrementCounter(counter: Ref[IO, Int], signal: Deferred[IO, Int]): IO[Unit] = for {
      _            <- IO.sleep(1.second)
      currentValue <- counter.updateAndGet(_ + 1)
      _            <- IO(s"Incremented value is $currentValue").debug
      _ <-
        if (currentValue >= 10) signal.complete(currentValue)
        else incrementCounter(counter, signal)
    } yield ()

    def printMessage(signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO("Message printer waits for signal").debug
      _ <- signal.get
      _ <- IO("time's up!").debug
    } yield ()

    for {
      counter    <- Ref[IO].of(0)
      signal     <- IO.deferred[Int]
      counterFib <- incrementCounter(counter, signal).start
      waiterFib  <- printMessage(signal).start
      _          <- counterFib.join *> waiterFib.join
    } yield ()
  }

  /**   - mega hard: implement racePair with Deffered
    *   - use a Deferred which can hold on Either[outcome for ioa, outcome for iob]
    *   - start two fibers, one for each IO
    *   - on completion (with any status), each IO needs to complete that Deferred
    *
    * (hint: use a finalizer from the Resources lesson) (hint2: use a guarantee call to make sure
    * the fibers complete the Deferred)
    *   - what do you do in case of cancellation (the hardest part)?
    */

  def ourRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]),
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B])
  ]] = ???

  override def run: IO[Unit] = alarm

}
