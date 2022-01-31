package com.rockthejvm.part4PolymorphicEffects

import com.rockthejvm._

import cats.effect.IOApp
import cats.effect.IO

import cats.syntax.parallel._
import scala.concurrent.duration._
import cats.effect.Ref

object Refs extends IOApp.Simple {

  def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L
    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- IO(ticks += 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").debug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  def tickingClockPure(): IO[Unit] = {
    def tickingClock(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Long]): IO[Unit] = for {
      _     <- IO.sleep(5.seconds)
      count <- ticks.get
      _     <- IO(s"TICKS: ${count}").debug
      _     <- printTicks(ticks)
    } yield ()

    for {
      ticks <- Ref[IO].of(0L)
      _     <- (tickingClock(ticks), printTicks(ticks)).parTupled
    } yield ()

  }

  override def run: IO[Unit] = tickingClockPure()
}
