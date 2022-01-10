package com.rockthejvm.part2effects

import java.util.Scanner
import scala.io.StdIn
import cats.effect.IO
import cats.syntax.apply._
import cats.syntax._

object IOIntroduction {

  /** Exercises
    */

  // 1. Sequence both IOs and return the LAST one
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  // 2. Sequence both IOs and return the FIRST one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = for {
    result <- ioa
    _ <- iob
  } yield result

  // 3. Repeat an IO effect forever
  def forever[A](io: IO[A]): IO[A] = io flatMap (_ => forever(io))

  // 4. Convert an IO to a different type
  def convert[A, B](io: IO[A], value: B): IO[B] = io.map(_ => value)

  def asUnit[A](io: IO[A]): IO[Unit] = io.map(_ => ())
  def asUnitV2[A](io: IO[A]): IO[Unit] = io.void

  def sumIO(num: Int): IO[Int] = {
    def sum(num: Int, aggr: IO[Int]): IO[Int] = {
      if (num == 0) aggr
      else sum(num - 1, aggr.map(_ + num))
    }

    sum(num, IO.pure(0))
  }

  def sumIOV2(num: Int): IO[Int] = {
    if (num <= 0) IO.pure(0)
    else
      for {
        lastNumber <- IO.pure(num)
        prevSum <- sumIO(num - 1)
      } yield prevSum + lastNumber
  }

  def fibonacci(n: Int): IO[BigInt] = n match {
    case 0 | 1 => IO.pure(n)
    case m     => (fibonacci(m - 1), fibonacci(n - 2)).mapN(_ + _)
  }

  def fibonacciFM(n: Int): IO[BigInt] = n match {
    case 0 | 1 => IO.pure(n)
    case m =>
      fibonacci(m - 1).flatMap(m1 => fibonacci(n - 2).map(m2 => m1 + m2))
  }

  def fibonacciHard(n: Int): IO[BigInt] = {
    if (n < 2) IO.pure(2)
    else
      for {
        last <- IO.defer(fibonacciHard(n - 1)) // same as .delay(...).flatten
        prev <- IO.defer(fibonacciHard(n - 2))
      } yield last + prev
  }

  def main(args: Array[String]): Unit = {
    implicit val runtime = cats.effect.unsafe.IORuntime.global
    (1 to 100).foreach(i => println(fibonacciHard(i).unsafeRunSync()))
  }
}
