package com.rockthejvm.part2effects

import scala.io.StdIn

object MyIO {
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  /** Exercises
    *   1. An IO which returns the current time of the system
    *
    * 2. An IO which measures the duration of a computation
    *
    * 3. An IO which prints something to the console
    *
    * 4. An IO which reads a line (a string) from the std input
    */

  def currentTime(): MyIO[Long] = MyIO(() => System.currentTimeMillis)

  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    start <- currentTime()
    _ <- computation
    end <- currentTime()
  } yield (end - start)

  def printStrLn(message: String): MyIO[Unit] = MyIO(() => println(message))

  def readStrLn(): MyIO[String] = MyIO(() => StdIn.readLine)

}
